#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>

#define MAX_SIZE 1024*1024*8

#define REGISTER_NUM 4
#define SAFE_BUFFER 64
#define EAX 0 // save IMM register 0
#define EBX 1 // register 1
#define ECX 2 // register 2
#define EDX 3 // register 3
#define EBP 8 // stack base ptr
typedef enum e_lexeme {
    IF = 128, Id, NUM, LOC, GLO,LPA,RPA,MUL,DIV,EQU,PLS,SUB,ASS,VAR,WHILE,ELSE,PRINT,STR,CIN
} lex;
typedef enum e_type {
    Variable,Pointer
} vartype;
typedef struct sym_table {
    char *id;
    lex lexeme;
    lex bind; //loc oe glo
    vartype type;
    int value;
} symtab;

// Global Var
char *source, *pos;
int line;
symtab *symbols, *symcur;
int token, token_value=0;
char* token_str;
int data_pos,data_top = REGISTER_NUM-1;
char *outpos, *result;
int esp=EBP;
// definition
void tokenize();
void Parse_expr();

char* getStr(char* begin, char* end){
    char* data = (char *)malloc(end-begin+1);
    memcpy(data,begin,end-begin);
    data[end-begin]='\0';
    return data;
}
void assert(int tk){
    if (token != tk){
        printf("Line:%d Syntax Error:%d should be %d",line,token,tk);
        exit(11);
    }
    tokenize();
}
// BF code
void addOUT(char op,int time){  // append to result
    for (int i = 0;i<time;i++){
        *outpos++=op;
    }
}
void inlOUT(char *code){
    for (char *i = code;*i!='\0';i++){
        *outpos++=(char)*i;
    }
}
void numOUT(int num){
    if(num>0)addOUT('+',num);
    else if(num<0)addOUT('-',-num);
    else return;
}
void clnOUT(){    // clean current position
    addOUT('[',1);
    addOUT('-',1);
    addOUT(']',1);
}
void jmpOUT(int pos){  // jmp to pos
    if (data_pos>pos) {addOUT('<',data_pos-pos);data_pos=pos;}
    else if (data_pos<pos) {addOUT('>',pos-data_pos);data_pos=pos;}
    else return;
}
void jnmOUT(int pos,int num){ //pos += num
    jmpOUT(pos);
    numOUT(num);
}
void movOUT(int pos1,int pos2){//pos1(cnnot be EDX) = pos2
    if (pos1==pos2) return;
    if (pos1==EDX){
        printf("EDX cannot use mov");
        exit(10);
    }
    //EDX[-]pos2[EDX+pos2-]pos1[-]EDX[pos1+pos2+EDX-]

    int frame_pos = data_pos;  // pos_frame
    jmpOUT(EDX);
    clnOUT();
    jmpOUT(pos2);
    addOUT('[',1);           //while begin
        jnmOUT(EDX,1);
        jnmOUT(pos2,-1);
    addOUT(']',1);           //while end
    jmpOUT(pos1);
    clnOUT();
    jmpOUT(EDX);
    addOUT('[',1);           //while begin
        jnmOUT(pos1,1);
        jnmOUT(pos2,1);
        jnmOUT(EDX,-1);
    addOUT(']',1);           //while end
    jmpOUT(frame_pos); // pos_frame
}
void pusOUT(int pos){
    // eip = EAX
    movOUT(esp,pos);
    esp+=1;
}
void pulOUT(int pos){
    esp-=1;
    movOUT(pos,esp);
}
void srgOUT(){ // push registers into stack (except edx)
    pusOUT(EAX);
    pusOUT(EBX);
    pusOUT(ECX);

}
void lrgOUT(){ // pull registers into stack (except edx)

    pulOUT(ECX);
    pulOUT(EBX);
    pulOUT(EAX);
}
void priOUT(int value){
    jmpOUT(SAFE_BUFFER+1);
    clnOUT();
    jmpOUT(SAFE_BUFFER);
    clnOUT();
    if(value>=64){// >++++++[<++++++>-]<
        addOUT('>',1);
        addOUT('+',8);
        addOUT('[',1);
        addOUT('<',1);
        addOUT('+',8);
        addOUT('>',1);
        addOUT('-',1);
        addOUT(']',1);
        addOUT('<',1);
        value-=64;
    }
    while (value>=36){
        addOUT('>',1);
        addOUT('+',6);
        addOUT('[',1);
        addOUT('<',1);
        addOUT('+',6);
        addOUT('>',1);
        addOUT('-',1);
        addOUT(']',1);
        addOUT('<',1);
        value-=36;
    }
    while (value>=16){
        addOUT('>',1);
        addOUT('+',4);
        addOUT('[',1);
        addOUT('<',1);
        addOUT('+',4);
        addOUT('>',1);
        addOUT('-',1);
        addOUT(']',1);
        addOUT('<',1);
        value-=16;
    }
    numOUT(value);
    addOUT('.',1);
}

//void gtoOUT(int pos){ // goto
//    data_pos=pos;
//    jmpOUT(data_pos);
//
//}
// IO

int readSRC(char * path){
    int fd, cnt;
    // 用 open/read/close 直接读取源码
    if ((fd = open(path, 0)) < 0) {
        printf("could not open source code(%s)\n", path);
        return -1;
    }
    if (!(source = pos = malloc(MAX_SIZE))) {
        printf("could not malloc(%lld) for source code\n", MAX_SIZE);
        return -1;
    }
    if ((cnt = read(fd, source, MAX_SIZE - 1)) <= 0) {
        printf("could not read source code(%lld)\n", cnt);
        return -1;
    }
    source[cnt] = 0; // 以 NUL 结尾
    close(fd);
}
// Parser

void tokenize(){
    char * id_ptr,* name;
    int i;
    while ((token = *pos++)){
        if (token == '\n'){line+=1;}// record line
        else if ((token >= 'a' && token <= 'z') || (token >= 'A' && token <= 'Z') || (token == '_')){// handle Identity
            id_ptr = pos-1;
            while ((*pos >= 'a' && *pos <= 'z') || (*pos >= 'A' && *pos <= 'Z')  // BUG: 首字母不能相同
                   || (*pos >= '0' && *pos <= '9') || (*pos == '_')){
                pos++;
            }
            name = getStr(id_ptr,pos);
            for (i = 0;symbols[i].id;i++){// search symbol
                if(!memcmp(name,symbols[i].id, strlen(symbols[i].id)) && !memcmp(name,symbols[i].id, pos-id_ptr)){
                    token = symbols[i].lexeme;
                    symcur=&symbols[i];
                    free(name);
                    return;
                }
            }
            printf("New Identity:%s\n", name);
            token=Id;
            symbols[i].id=name;
            symbols[i].lexeme=Id;
            symcur=&symbols[i];
            return;
        }
        else if (token == '"' ){
            id_ptr=pos;
            while (*pos!='"'){
                pos++;
            }
            token_str= getStr(id_ptr,pos);
            pos++;
            token=STR;
            return;
        }
        else if (token >= '0' && token <= '9'){// handle number
            // 十进制（非 0 开头）
            if ((token_value = token - '0'))
                while (*pos >= '0' && *pos <= '9') token_value = token_value * 10 + *pos++ - '0';
                // 16 进制 0x 或 0X
            else if (*pos == 'x' || *pos == 'X')
                while ((token = *++pos) && ((token >= '0' && token <= '9') || (token >= 'a' && token <= 'f')
                                            || (token >= 'A' && token <= 'F')))
                    // 这里的写法有点简化：把字符 & 0xF 加上字母校正 9（有点魔法）
                    token_value = token_value * 16 + (token & 0xF) + (token >= 'A' ? 9 : 0);
                // 八进制（以 0 开头而后接 0-7）
            else while (*pos >= '0' && *pos <= '7') token_value = token_value * 8 + *pos++ - '0';
            token=NUM;
            return;
        }
        else if (token == '('){
            token=LPA;
            return;
        }
        else if (token == ')'){
            token=RPA;
            return;
        }
        else if (token == '*') {token = MUL; return;}
        else if (token == '+') {token = PLS; return;}
        else if (token == '-') {token = SUB; return;}
        else if (token == '=') {token = ASS; return;}
        else if (token == '~' || token == ';' || token == '{' || token == '}'  || token == ']' || token == ',' || token == ':') return;
    }
}

void initSymbols(){
    symbols[0].id="if";
    symbols[0].lexeme=IF;
    symbols[1].id="var";
    symbols[1].lexeme=VAR;
    symbols[2].id="while";
    symbols[2].lexeme=WHILE;
    symbols[3].id="else";
    symbols[3].lexeme=ELSE;
    symbols[4].id="print";
    symbols[4].lexeme=PRINT;
    symbols[5].id="cin";
    symbols[5].lexeme=CIN;

}
void Parse_factor(){
    int var_pos;
    if(token==NUM){
        assert(NUM);
        // push token_value
        jmpOUT(EAX);
        clnOUT();
        numOUT(token_value);
        pusOUT(EAX);
    }
    else if(token==LPA){
        assert(LPA);
        Parse_expr();
        assert(RPA);
    }
    else if(token==CIN){
        assert(CIN);
        assert(LPA);
        assert(RPA);
        jmpOUT(EAX);
        addOUT(',',1);
        pusOUT(EAX);
    }
    else if(token==Id){
        assert(Id);
        pusOUT(symcur->value);
        jmpOUT(symcur->value);

    }
    else{
        printf("Syntax Error line:%d",line);
    }
}
void Parse_term(){
    int frame;
    Parse_factor();
    while (token==MUL || token==DIV){
        if(token==MUL){           // data_pos * data_pos(EAX)|data_pos
            assert(MUL);
            Parse_factor(); //get another MUL num
            // get right
            // pull ecx (right)
            // pull ebx (left)
            // EAX[-]EDX[-]EBX[ECX[EAX+EDX+ECX-]EDX[ECX+EDX-]EBX-]
            frame=data_pos;
            pulOUT(ECX);
            pulOUT(EBX);
            jmpOUT(EAX);
            clnOUT();
            jmpOUT(EDX);
            clnOUT();
            jmpOUT(EBX);
            addOUT('[',1);           //while begin
                jmpOUT(ECX);
                addOUT('[',1);           //while begin
                    jnmOUT(EAX,1);
                    jnmOUT(EDX,1);
                    jnmOUT(ECX,-1);
                addOUT(']',1);          //while end
            jmpOUT(EDX);
                addOUT('[',1);           //while begin
                    jnmOUT(ECX,1);
                    jnmOUT(EDX,-1);
                addOUT(']',1);          //while end
                jnmOUT(EBX,-1);
            addOUT(']',1);            //while end
            // result
            pusOUT(EAX);
            jmpOUT(frame);
        }
    }

}
void Parse_expr(){

    Parse_term();// save in EAX
    while (token==PLS || token==SUB){ // data_pos + data_pos{num} | data_pos(term) | data_pos(var)

        if(token==PLS){
            assert(PLS);
            Parse_term();
            // pull ebx (right
            // pull eax {left
            // eax(left) + ebx(right)
            // EBX[EAX+EBX-]
            pulOUT(EBX);
            pulOUT(EAX);
            jmpOUT(EBX);
            addOUT('[',1);           //while begin
                jnmOUT(EAX,1);
                jnmOUT(EBX,-1);
            addOUT(']',1);          //while end

            //result
            pusOUT(EAX);
        }
        else if (token==SUB){
            assert(SUB);
            Parse_term();
            // pull ebx (right
            // pull eax {left
            // eax(left) - ebx(right)
            // EBX[EAX-EBX-]
            pulOUT(EBX);
            pulOUT(EAX);
            jmpOUT(EBX);
            addOUT('[',1);           //while begin
                jnmOUT(EAX,-1);
                jnmOUT(EBX,-1);
            addOUT(']',1);          //while end

            //result
            pusOUT(EAX);
        }
    }

}

void Parse_stmt(){
    int tmp_pos;
    int stack_pos;
    if (data_top>=EBP) {printf("Stack out of range");exit(12);}
    if (data_top>=SAFE_BUFFER) {printf("SAFE_BUFFER is not safe");exit(12);}

    stack_pos=esp;

    if(token==ASS){
        tmp_pos=symcur->value;
        assert(ASS);
        Parse_expr();
        assert(';');
        pulOUT(tmp_pos);
        return;
    }
    else if(token==WHILE){
        assert(WHILE);
        assert(LPA);
        assert(Id);
        tmp_pos=symcur->value;
        assert(RPA);
        jmpOUT(tmp_pos);
        addOUT('[',1);           //while begin
        Parse_stmt();
        jmpOUT(tmp_pos);
        addOUT(']',1);           //while emd
    }
    else if(token == IF){



        assert(IF);
        assert(LPA);
        Parse_expr();
        assert(RPA);
        pulOUT(EAX);

        jmpOUT(EBX);//        EBX[-]
        clnOUT();
        jmpOUT(ECX);//        ECX[-]
        clnOUT();

        jmpOUT(EAX);//        x[EBX+ECX+x-]   EBX[x+EBX-]+
        addOUT('[',1);           //while begin
        jmpOUT(EBX);
        numOUT(1);
        jmpOUT(ECX);
        numOUT(1);
        jmpOUT(EAX);
        numOUT(-1);
        addOUT(']',1);           //while emd

        jmpOUT(EBX);
        addOUT('[',1);           //while begin
        jmpOUT(EAX);
        numOUT(1);
        jmpOUT(EBX);
        numOUT(-1);
        addOUT(']',1);           //while emd
        numOUT(1);

        jmpOUT(ECX);//                          ECX[
        addOUT('[',1);           //IF begin

//        stack_pos=esp;
//        srgOUT();
        pusOUT(EBX);
        pusOUT(ECX);
        Parse_stmt();//        code1
        pulOUT(ECX);
        pulOUT(EBX);
//        esp=stack_pos;
//        lrgOUT();


        jmpOUT(EBX);//        EBX-
        numOUT(-1);

        jmpOUT(ECX);//        ECX[-]]
        clnOUT();

        addOUT(']',1);           //IF end



        if(token==ELSE){
            assert(ELSE);

            jmpOUT(EBX);//        EBX[
            addOUT('[',1);           //ELSE begin

            Parse_stmt();//        code2

            jmpOUT(EBX);//        EBX-]
            numOUT(-1);
            addOUT(']',1);           //ELSE end

        }

    }
    else if(token==PRINT){
        assert(PRINT);
        assert(LPA);
        if(token==STR){
            assert(STR);
            for(char* i = token_str;*i!='\0';i++){
                priOUT(*i);
            }
        } else {
            assert(Id);
//            tmp_pos=data_pos;
            movOUT(SAFE_BUFFER,symcur->value);
            jmpOUT(SAFE_BUFFER);
            inlOUT(">>++++++++++<<[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++++++<]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<");
//            jmpOUT(tmp_pos);
            priOUT('\n');
        }

        assert(RPA);
        assert(';');
    }
    else if (token==VAR){
        tokenize();
        assert(Id);

        data_top+=1;  // set ptr to the top of data
        symcur->bind=GLO;
        symcur->value=data_top;
        symcur->type=Variable;
    }
    else if(token=='{'){
        assert('{');
        while (token!='}'){
            Parse_stmt();
        }
        assert('}');
    }
    else if (token==Id) assert(Id);
    else if (token==';') assert(';');
    else Parse_expr();



}
void Parse(){
    line = 1;
    readSRC("E:/Code/cpp/BFplus/test.bfp");
    symbols = (symtab *) calloc(128, sizeof(symtab));
    result = outpos = (char *) calloc(MAX_SIZE, 1);
    initSymbols();
    printf("%s\n", source);
    token = 1;line = 1;
    tokenize();
    while (token) {
        Parse_stmt();
    }
}



int main(int argc, char **argv) {
    Parse();

    printf("%s",result);
    return 0;
}
