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
    matrix_sf *product = (matrix_sf *)malloc(sizeof(matrix_sf) + mat1->num_cols * mat1->num_rows * sizeof(int));
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
    matrix_sf *transpose = (matrix_sf *)malloc(sizeof(matrix_sf) + mat->num_cols  *mat->num_rows * sizeof(int));
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

    char *ptr;
    char *curr = strtok_r((char *)expr_copy, "=", &ptr);

    /* Capture number of rows */
    curr = strtok_r(NULL, " \t\n", &ptr);
    unsigned int rows = atoi(curr);

    /* Capture number of columns */
    curr = strtok_r(NULL, " \t\n", &ptr);
    unsigned int cols = atoi(curr);

    /* Capture the values */
    curr = strtok_r(NULL, "[", &ptr);

    int *vals = (int *)malloc(rows * cols * sizeof(int));
    for (unsigned int i = 0; i < rows * cols; i++) {
        while (*curr && !isdigit((unsigned char)*curr) && *curr != '-') {
            curr++;
        }
        vals[i] = strtol(curr, &curr, 10);
    }

    /* Create matrix */
    matrix_sf *mat = (matrix_sf *)malloc(sizeof(char) + 2 * sizeof(unsigned int) + sizeof(int) * rows * cols);
    mat->name = name;
    mat->num_cols = cols;
    mat->num_rows = rows;
    memcpy(mat->values, vals, rows * cols * sizeof(int));

    return mat;
}

char* infix2postfix_sf(char *infix) {
    unsigned int len = strlen(infix);
    char* postfix = (char *)malloc((len + 1) * sizeof(char));
    char *stack = (char *)malloc((len + 1) * sizeof(char));
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
    free(stack);
    return postfix;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char *postfix = infix2postfix_sf(expr);
    printf("Postfix:%s\n", postfix);
    unsigned int len = strlen(postfix);
    matrix_sf **stack = (matrix_sf **)malloc((len + 1) * sizeof(matrix_sf));
    int top = 0;

    char *curr = postfix;
    while (*curr != '\0') {
        if ((*curr >= 'A' && *curr <= 'Z') || (*curr >= 'a' && *curr <= 'z')) {
            printf("Found a letter:\n");
            matrix_sf *operand = find_bst_sf(curr[0], root);
            stack[++top] = operand;
            print_matrix_sf(operand);
        }
        else if ((*curr == '+') || (*curr == '*') || (*curr == '\'')) {
            printf("Found a sign:\n");
            matrix_sf **result;
            switch(*curr) {
                matrix_sf **operand1, **operand2;
                case '+': 
                    operand2 = &stack[top--];
                    operand1 = &stack[top--];
                    print_matrix_sf(*operand1);
                    print_matrix_sf(*operand2);

                    result = add_mats_sf(*operand1, *operand2);
                    print_matrix_sf(result);
                    break;
                case '*':
                    operand2 = &stack[top--];
                    operand1 = &stack[top--];
                    print_matrix_sf(*operand1);
                    print_matrix_sf(*operand2);

                    result = mult_mats_sf(*operand1, *operand2);
                    break;
                case '\'':
                    print_matrix_sf(*operand1);

                    result = transpose_mat_sf(stack[top--]);
                    break;
            }
            stack[++top] = result;
        }
        curr++;
    }
    matrix_sf **evaluation = &stack[top--];
    free(postfix);
    free(stack);
    return *evaluation;
}

matrix_sf *execute_script_sf(char *filename) {
    FILE *file = fopen(filename, "r");

    bst_sf *bst = NULL;
    char *line = NULL;
    size_t max = MAX_LINE_LEN;
    while (getline(&line, &max, file) != -1) {
        char name, *ptr, *expr;
        char *curr = strtok_r(line, "", &ptr);
        curr = strtok_r(NULL, "=", &ptr);
        name = curr[0];

        curr = strtok_r(NULL, "", &ptr);
        expr = curr;
        if (strstr(line, "[") != NULL) {
            matrix_sf *mat = create_matrix_sf(name, (const char*)expr);
            bst = insert_bst_sf(mat, bst);
        } else {
            matrix_sf *result = evaluate_expr_sf(name, expr, bst);
            bst = insert_bst_sf(result, bst);
        }
        free(line);
        line = NULL;
    }
    fclose(file);
    free_bst_sf(bst);
    return NULL;
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

void printBST(bst_sf *root, int level) {
    if (root != NULL) {
        printBST(root->right_child, level + 1);
        for (int i = 0; i < level; i++) {
            printf("    ");
        }
        printf("%c\n", root->mat->name);
        printBST(root->left_child, level + 1);
    }
}

// int main() {

//     const char* exprA = "A = 2 2 [1 2; 3 4;]";
//     matrix_sf* a = create_matrix_sf('A', exprA);
//     //print_matrix_sf(a);

//     const char* exprB = "A = 2 2 [5 6; 7 8;]";
//     matrix_sf* b = create_matrix_sf('B', exprB);
//     //print_matrix_sf(b);

//     const char* exprC = "A = 2 2 [9 10; 11 12;]";
//     matrix_sf* c = create_matrix_sf('C', exprC);
//     //print_matrix_sf(c);

//     const char* exprD = "A = 2 2 [13 14; 15 16;]";
//     matrix_sf* d = create_matrix_sf('D', exprD);
//     //print_matrix_sf(d);

//     bst_sf *root = NULL;

//     bst_sf *bst = insert_bst_sf(a, root);
//     bst = insert_bst_sf(b, bst);
//     bst = insert_bst_sf(c, bst);
//     bst = insert_bst_sf(d, bst);

   
//     // char * expr =  "(A +B)\' * C * (D\' + A)\' \n";
//     //print_matrix_sf(add_mats_sf(a, b));
//     char * expr =  "(A+B) + C";
//     matrix_sf *eval = evaluate_expr_sf('Z', expr, bst);
//     print_matrix_sf(eval);

// }