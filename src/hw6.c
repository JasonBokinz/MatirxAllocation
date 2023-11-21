#include "../include/hw6.h"

int orderOfOperations(char operator) {
    if (operator == '\'') {
        return 2;
    } else if ((operator == '*') || (operator == '/')) {
        return 1;
    } else if ((operator == '+') || (operator == '-')) {
        return 0;
    }
    return -1;
}

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    bst_sf *newNode = (bst_sf *)malloc(sizeof(bst_sf));
    newNode->mat = mat;
    newNode->left_child = NULL;
    newNode->right_child = NULL;

    if (!root) { return newNode; }

    bst_sf *curr = root;
    while(1) {
        if (mat->name < curr->mat->name) {
            if (!curr->left_child) {
                curr->left_child = newNode;
                return root;
            } else {
                curr = curr->left_child;
            }
        } else if (mat->name > curr->mat->name) {
            if (!curr->right_child) {
                curr->right_child = newNode;
                return root;
            } else {
                curr = curr->right_child;
            }
        }
    }
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if (!root) { 
        return NULL; 
    }

    if (root->mat->name == name) { 
        return root->mat; 
    } else if (name < root->mat->name) {
        return find_bst_sf(name, root->left_child);
    } else {
        return find_bst_sf(name, root->right_child);
    }
}

void free_bst_sf(bst_sf *root) {
    if (root) {
        free_bst_sf(root->left_child);
        free_bst_sf(root->right_child);
        free(root->mat);
        free(root);
    }
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
   matrix_sf *sum = (matrix_sf *)malloc(sizeof(matrix_sf) + mat1->num_cols * mat1->num_rows * sizeof(int));
    sum->name = '?';
    sum->num_cols = mat2->num_cols;
    sum->num_rows = mat2->num_rows;

    for (unsigned int i = 0; i < mat1->num_rows; i++) {
        for (unsigned int j = 0; j < mat1-> num_cols; j++) {
            sum->values[i * mat1->num_cols + j] = mat1->values[i * mat1->num_cols + j] + mat2->values[i * mat2->num_cols + j];
        }
    }
    return sum;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    matrix_sf *product = (matrix_sf *)malloc(sizeof(matrix_sf) + mat1->num_rows * mat2->num_cols * sizeof(int));
    product->name = '?';
    product->num_cols = mat2->num_cols;
    product->num_rows = mat1->num_rows;

    for (unsigned int i = 0; i < mat1->num_rows; i++) {
        for (unsigned int j = 0; j < mat2-> num_cols; j++) {
            unsigned int columnSum = 0;
            for (unsigned int k = 0; k < mat1->num_cols; k++) {
                columnSum += mat1->values[i * mat1->num_cols + k] * mat2->values[k * mat2->num_cols + j];
            }
            product->values[i * mat2->num_cols + j] = columnSum;
        }
    }
    return product;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    matrix_sf *transpose = (matrix_sf *)malloc(sizeof(matrix_sf) + mat->num_cols * mat->num_rows * sizeof(int));
    transpose->name = '?';
    transpose->num_rows = mat->num_cols;
    transpose->num_cols = mat->num_rows;

    for (unsigned int  i = 0; i < mat->num_rows; i++) {
        for (unsigned int j = 0; j < mat->num_cols; j++) {
            transpose->values[j * mat->num_rows + i] = mat->values[i * mat->num_cols + j];
        }
    }
    return transpose;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    char *expr_copy = (char *)malloc(strlen(expr) + 1);
    strcpy(expr_copy, expr);

    char *ptr, *curr;
    if (strstr(expr, "=") != NULL) {
        curr = strtok_r((char *)expr_copy, "=", &ptr);
        curr = strtok_r(NULL, " \t\n", &ptr);
    } else {
        curr = strtok_r((char *)expr_copy, " \t\n", &ptr);
    }

    /* Capture number of rows */
    unsigned int rows = atoi(curr);

    /* Capture number of columns */
    curr = strtok_r(NULL, "[", &ptr);
    unsigned int cols = atoi(curr);
    curr = strtok_r(NULL, "", &ptr);


    int *vals = (int *)malloc(rows * cols * sizeof(int));
    for (unsigned int i = 0; i < rows * cols; i++) {
        while (*curr && !isdigit((unsigned char)*curr) && *curr != '-') {
            curr++;
        }
        vals[i] = strtol(curr, &curr, 10);
    }

    /* Create matrix */
    matrix_sf *mat = (matrix_sf *)malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    mat->name = name;
    mat->num_cols = cols;
    mat->num_rows = rows;
    memcpy(mat->values, vals, rows * cols * sizeof(int));
    free(vals);
    free(expr_copy);
    return mat;
}

char* infix2postfix_sf(char *infix) {
    unsigned int len = strlen(infix);
    char* postfix = (char *)malloc((len + 1) * sizeof(char));
    char stack[MAX_LINE_LEN];
    int top = 0;
    int index = 0;

    for (unsigned int i = 0; i < len; i++) {
        char ch = infix[i];
        if (isalnum(ch)) {
            postfix[index++] = ch;
        } else if (ch == '(') {
            stack[++top] = ch;
        } else if (ch == ')') {
            while ((top != 0) && (stack[top] != '(')){
                postfix[index++] = stack[top--];
            }
            top--;
        } else if ((ch == '+') || (ch == '*') || (ch == '\'')) {
            while ((top != 0) && (orderOfOperations(ch) <= orderOfOperations(stack[top]))) {
                postfix[index++] = stack[top--];
            }
            stack[++top] = ch;
        }
    }
    while (top != 0) {
        postfix[index++] = stack[top--];
    }
    postfix[index] = '\0';
    return postfix;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char *postfix = infix2postfix_sf(expr);
    matrix_sf *stack[MAX_LINE_LEN];
    int top = 0;

    char *curr = postfix;
    matrix_sf *result = NULL;

    while (*curr != '\0') {
        if ((*curr >= 'A' && *curr <= 'Z') || (*curr >= 'a' && *curr <= 'z')) {
            matrix_sf *operand = find_bst_sf(curr[0], root);
            stack[++top] = operand;
        }
        else if ((*curr == '+') || (*curr == '*') || (*curr == '\'')) {
            matrix_sf *operand1 = NULL;
            matrix_sf *operand2 = NULL;
            switch(*curr) {
                case '+': 
                    operand2 = stack[top--];
                    operand1 = stack[top--];

                    result = add_mats_sf(operand1, operand2);
                    break;
                case '*':
                    operand2 = stack[top--];
                    operand1 = stack[top--];

                    result = mult_mats_sf(operand1, operand2);
                    break;
                case '\'':
                    operand1 = stack[top--];

                    result = transpose_mat_sf(operand1);
                    break;
            }
            stack[++top] = result;
            if (operand1 && !isalpha(operand1->name)) {
                free(operand1);
            }
            if (operand2 && !isalpha(operand2->name)) {
                free(operand2);
            }
        }
        curr++;
    }
    result = stack[top--];
    matrix_sf *finalResult = copy_matrix(result->num_rows, result->num_cols, result->values);
    finalResult->name = name;
    
    free(result);
    free(postfix);
    return finalResult;
}

matrix_sf *execute_script_sf(char *filename) {
    FILE *file = fopen(filename, "r");

    bst_sf *bst = NULL;
    char *line = NULL;
    size_t max = MAX_LINE_LEN;
    matrix_sf *result = NULL;
    while (getline(&line, &max, file) != -1) {
        char name = '\0';
        char *ptr = NULL;
        char *expr = NULL;

        char *curr = strtok_r(line, "=", &ptr);
        name = curr[0];

        curr = strtok_r(NULL, "", &ptr);
        expr = curr;

        if (strstr(expr, "[") != NULL) {
            matrix_sf *mat = create_matrix_sf(name, (const char*)expr);
            bst = insert_bst_sf(mat, bst);
        } else {
            result = evaluate_expr_sf(name, expr, bst);
            bst = insert_bst_sf(result, bst);
        }
        free(line);
        line = NULL;
    }
    free(line);
    fclose(file);
    matrix_sf *resultCopy = copy_matrix(result->num_rows, result->num_cols, result->values);
    resultCopy->name = result->name;
    free_bst_sf(bst);
    return resultCopy;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}