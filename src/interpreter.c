#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LINE_BUFFER_SIZE 30
#define PROGRAM_SIZE 100
#define STACK_SIZE 100

// Parsing
typedef enum
{
    PUSH,
    ADD,
    SUB,
    MUL,
    DIV,
    LOAD,
    STORE
} Bytecode;

typedef struct
{
    Bytecode instruction;
    union {
        double number;
        char* name;
    };
} Instruction;

Instruction parseInstruction(char *line)
{
    Instruction instr;
    if (sscanf(line, "PUSH %lf", &instr.number))
    {
        printf("matched PUSH %lf\n", instr.number);
        instr.instruction = PUSH;
    }
    else if (strncmp(line, "ADD", 3) == 0)
    {
        printf("matched ADD\n");
        instr.instruction = ADD;
    }
    else if (strncmp(line, "SUB", 3) == 0)
    {
        printf("matched SUB\n");
        instr.instruction = SUB;
    }
    else if (strncmp(line, "MUL", 3) == 0)
    {
        printf("matched MUL\n");
        instr.instruction = MUL;
    }
    else if (strncmp(line, "DIV", 3) == 0)
    {
        printf("matched DIV\n");
        instr.instruction = DIV;
    }
    else if (strncmp(line, "LOAD", 4) == 0)
    {
        instr.instruction = LOAD;\
        size_t t = strcspn(line + 6, "\"");
        instr.name = strndup(line + 6, t);
        printf("matched LOAD %s\n", instr.name);
    }
    else if (strncmp(line, "STORE", 5) == 0)
    {
        instr.instruction = STORE;
        size_t t = strcspn(line + 7, "\"");
        instr.name = strndup(line + 7, t);
        printf("matched STORE %s\n", instr.name);
    }
    else
    {
        fprintf(stderr, "Unknown instruction: %s\n", line);
        exit(EXIT_FAILURE);
    }
    return instr;
}

// Variable store
typedef struct {
    char* names[PROGRAM_SIZE];
    double values[PROGRAM_SIZE];
    int count;
} VarTable;

// Stack
typedef struct
{
    double data[STACK_SIZE];
    int top;
} Stack;

void initStack(Stack *stack)
{
    stack->top = -1;
}

void push(Stack *stack, double value)
{
    stack->data[++stack->top] = value;
}

double pop(Stack *stack)
{
    return stack->data[stack->top--];
}

double execute(Instruction *program, int instructionCount)
{
    printf("\nExecuting program...\n");
    Stack stack;
    initStack(&stack);
    VarTable vars;

    for (int i = 0; i < instructionCount; i++)
    {
        Instruction instr = program[i];
        switch (instr.instruction)
        {
        case PUSH:
        {
            push(&stack, instr.number);
            printf("pushing %lf\n", instr.number);
            break;
        }

        case ADD:
        {
            double b = pop(&stack);
            double a = pop(&stack);
            printf("adding %lf to %lf\n", a, b);
            push(&stack, a + b);
            break;
        }

        case SUB:
        {
            double b = pop(&stack);
            double a = pop(&stack);
            printf("subtracting %lf from %lf\n", b, a);
            push(&stack, a - b);
            break;
        }

        case MUL:
        {
            double b = pop(&stack);
            double a = pop(&stack);
            printf("multiplying %lf by %lf\n", a, b);
            push(&stack, a * b);
            break;
        }

        case DIV:
        {
            double b = pop(&stack);
            double a = pop(&stack);
            printf("dividing %lf by %lf\n", a, b);
            if (b == 0)
            {
                fprintf(stderr, "Error: Division by zero\n");
                exit(EXIT_FAILURE);
            }
            push(&stack, a / b);
            break;
        }

        case STORE:
        {
            if (vars.count >= PROGRAM_SIZE)
            {
                fprintf(stderr, "Error: Variable table full\n");
                exit(EXIT_FAILURE);
            }
            double value = pop(&stack);
            vars.names[vars.count] = instr.name;
            vars.values[vars.count] = value;
            vars.count++;
            printf("storing %lf in %s\n", value, instr.name);
            break;
        }

        case LOAD:
        {
            int found = 0;
            for (int i = 0; i < vars.count; i++)
            {
                if (strcmp(vars.names[i], instr.name) == 0)
                {
                    push(&stack, vars.values[i]);
                    printf("loading %s: %lf\n", instr.name, vars.values[i]);
                    break;
                }
            }

            if (!found)
            {
                fprintf(stderr, "Error: Variable %s not found\n", instr.name);
                exit(EXIT_FAILURE);
            }
        }

        default:
        {
            fprintf(stderr, "Unknown instruction\n");
            exit(EXIT_FAILURE);
        }
        }
    }

    return pop(&stack);
}

int main()
{
    FILE *fptr = fopen("bytecode.txt", "r");
    printf("\nStarting interpreter...\n");

    Instruction *program = malloc(PROGRAM_SIZE * sizeof(Instruction));
    int instructionCount = 0;

    char line[LINE_BUFFER_SIZE];
    while (fgets(line, sizeof(line), fptr))
    {
        printf("\nline: %s", line);
        Instruction instr = parseInstruction(line);
        program[instructionCount++] = instr;
    }

    fclose(fptr);

    double result = execute(program, instructionCount);
    printf("Result: %lf\n", result);

    free(program);
    return 0;
}
