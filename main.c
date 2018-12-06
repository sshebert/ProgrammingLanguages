#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct node{
    char value;
    struct node *next, *prev;
}node;

typedef struct state{
    char writeVal, moveDir;
    int nextState;
}state;

typedef struct string{
    char str[100];
}string;

//region Globals

//list
node *head, *tail, *curr;
int listCount = 0;

//input file
FILE *file;
int charMax = 100;
char fileDir[] = "C:\\Users\\samue\\Desktop\\Assignment\\";
char fileName[100], tapeStr[100], buffer[100];
int states, currState, endState;

//turing machine
char readVal;

//endregion

//region Doubly Linked List

node* CreateNode(char val){
    node *temp = malloc(sizeof(node));
    temp->value = val;
    temp->next = NULL;
    temp->prev = NULL;
    return temp;
}

void Append(char val){

    if(tail == NULL){
        tail = CreateNode(val);
        head = tail;
    }
    else {
        tail->next = CreateNode(val);
        tail->next->prev = tail;
        tail = tail->next;
    }
    listCount++;
}

void Prepend(char val){

    if(head == NULL){
        head = CreateNode(val);
        tail = head;
    }
    else {
        head->prev = CreateNode(val);
        head->prev->next = head;
        head = head->prev;
    }
    listCount++;
}

void MoveLeft(){
    if(curr == head){
        Prepend('B');
        curr = head;
    }
    else{
        curr = curr->prev;
    }
}

void MoveRight(){
    if(curr == tail){
        Append('B');
        curr = tail;
    }
    else{
        curr = curr->next;
    }
}

void PrintList(){

    node *temp = head;
    while(temp != NULL){
        printf("%c",temp->value);
        temp = temp->next;
    }
    printf("\n");
}
//endregion

int main() {

    //region Read File

    //file name
    printf("Input File:\n");
    scanf("%s", fileName);
    //printf("%s\n", fileName);
    strcat(fileDir, fileName);
    //printf("%s\n", fileDir);

    //open file
    file = fopen(fileDir, "r");
    if (!file) {
        printf("Could not open file");
        return 1;
    }
    printf("Writing tape...\n");

    //tape content
    fgets(tapeStr, charMax, file);
    //printf("%s\n", tapeStr);

    //states
    fgets(buffer, charMax, file);
    states = atoi(buffer);
    //printf("%d\n", states);

    //start state
    fgets(buffer, charMax, file);
    currState = atoi(buffer);
    //printf("%d\n", currState);

    //end state
    fgets(buffer, charMax, file);
    endState = atoi(buffer);
    //printf("%d\n", endState);

    //instructions
    //store lines in "arraylist"
    int count = 0;
    int size = 10;
    string *tempArr = malloc(size * sizeof(string));
    while (fgets(buffer, charMax, file) != NULL) {
        strcpy(tempArr[count].str, buffer);
        if (count == size - 1) {
            size = size * 2;
            tempArr = realloc(tempArr, size * sizeof(string));
        }
        count++;
    }
    //for(int i = 0; i < count; i++){
        //printf("%s\n", tempArr[i].str);
    //}

    //endregion

    //region Set Turing Machine

    //create state table
    state **stateTable = malloc(states*sizeof(state*));
    for(int i = 0; i < states; i++){
        stateTable[i] = malloc(256*sizeof(state));
    }

    for(int i = 0; i < count; i++){
        int row, column, new;
        char *token, write, move;

        //first token state
        token = strtok(tempArr[i].str," ");
        row = atoi(token);

        //second token readval
        token = strtok(NULL, " ");
        column = (int)*token;//int cast of char is ascii value

        //third token writeval
        token = strtok(NULL, " ");
        write = *token;

        //fourth token movedirection
        token = strtok(NULL, " ");
        move = *token;

        //newstate
        token = strtok(NULL, " ");
        new = atoi(token);

        //add to state table
        stateTable[row][column].writeVal = write;
        stateTable[row][column].moveDir = move;
        stateTable[row][column].nextState = new;
    }
    printf("Initial tape contents: %s\n", tapeStr);

    //populate linked list
    count = 0;
    while((tapeStr[count] != '\n') && (tapeStr[count] != '\0') ){
        Append(tapeStr[count]);
        count++;
    }
    //PrintList();

    //set turing machine
    curr = head;

    //cleanup
    fclose(file);
    free(tempArr);

    //endregion

    //region Turing Machine

    while(currState != endState){
        //read value
        readVal = curr->value;

        //write new value
        curr->value = stateTable[currState][(int)readVal].writeVal;

        //move head
        if(stateTable[currState][(int)readVal].moveDir == 'R'){
            MoveRight();
        }
        else{
            MoveLeft();
        }

        //change state
        currState = stateTable[currState][(int)readVal].nextState;
    }

    printf("Final tape contents: ");
    PrintList();


    //endregion

    return 0;
}

