#include "hw7.h"

// my utils

typedef unsigned int uint;

matrix_sf* create_empty_mats_sf_from_dim(int R, int C) {
    matrix_sf *out = malloc(sizeof(matrix_sf) + R*C*sizeof(int));
    out->name = '?';
    out->num_rows = R;
    out->num_cols = C;
    return out;
}

bst_sf* create_leaf(matrix_sf *mat) {
    bst_sf *out = malloc(sizeof(bst_sf));
    out->mat = mat;
    out->left_child = NULL;
    out->right_child = NULL;
    return out;
}

int op_to_precendence(char op) {
    if (op == '+')
        return 0;
    if (op == '*')
        return 1;
    
    // note to self: unreachable
    return -1;
}


// assignment below

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (root == NULL)
        return create_leaf(mat);
    
    if (root->mat->name < mat->name) {
        root->right_child = insert_bst_sf(mat, root->right_child);
    } else {
        root->left_child = insert_bst_sf(mat, root->left_child);
    }
    // we don't have to worry about equal case

    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if (root == NULL)
        return NULL;
    
    if (root->mat->name < name)
        return find_bst_sf(name, root->right_child);
    if (root->mat->name > name)
        return find_bst_sf(name, root->left_child);
    
    // else they must be equal
    return root->mat;
}

void free_bst_sf(bst_sf *root) {
    if (root == NULL)
        return;
    
    free(root->mat);
    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    int R = mat1->num_rows, 
        C = mat1->num_cols;
    
    matrix_sf *out = create_empty_mats_sf_from_dim(R, C);

    int *mat1_p = mat1->values, 
        *mat2_p = mat2->values, 
        *out_p = out->values;
    uint i;
    for (; R>0; R--) {
        for (i=0; i<C; i++) {
            *out_p = *mat1_p + *mat2_p;
            out_p++;
            mat1_p++;
            mat2_p++;
        }
    }

    return out;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    int R = mat1->num_rows,
        T = mat1->num_cols,
        C = mat2->num_cols;

    matrix_sf *out = create_empty_mats_sf_from_dim(R, C);

    int *mat1_p = mat1->values, 
        *mat2_p = mat2->values, 
        *out_p = out->values;
    
    uint i, j, k;
    for (i=0; i<R; i++) {
        for (j=0; j<C; j++) {
            *out_p = 0;

            for (k=0; k<T; k++) {
                // mat1_p[i*T + k] gets row i, col k of mat1 [R by T]
                // mat2_p[k*C + j] gets row k, col j of mat2 [T by C]
                *out_p+= mat1_p[i*T + k]*mat2_p[k*C + j];
            }
            
            out_p++;
        }
    }
    
    return out;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    int R = mat->num_rows, 
        C = mat->num_cols;
    
    matrix_sf *out = create_empty_mats_sf_from_dim(C, R);

    int *mat_p = mat->values, 
        *out_p = out->values;
    uint i, j;
    for (i=0; i<R; i++) {
        for (j=0; j<C; j++) {
            // out_p[j, i] = mat_p[i, j];
            out_p[j*R + i] = mat_p[C*i + j];
        }
    }

    return out;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    // we need to create a copy so that we can actually strtok it
    char* expr_copy = calloc(strlen(expr) + 1, sizeof(char));
    strcpy(expr_copy, expr);

    // split expr into dimensions and matrix
    char *dim_str, *mat_str;
    assert((dim_str = strtok(expr_copy, "[")) != NULL);
    assert((mat_str = strtok(NULL, "]")) != NULL);

    // dim now has the 2 numbers
    // expr now has everything between []

    // parse dimensions
    char *temp;
    uint NR, NC; 
    assert((temp = strtok(dim_str, " ")) != NULL);
    assert(sscanf(temp, "%d", &NR) == 1);

    assert((temp = strtok(NULL, " ")) != NULL);
    assert(sscanf(temp, "%d", &NC) == 1);

    // printf("\nmat_str=\"%s\"\n", mat_str);

    // build out empty matrix
    matrix_sf *new_mat = create_empty_mats_sf_from_dim(NR, NC);
    new_mat->name = name;

    int *new_mat_p = new_mat->values;

    char *row_str, *row_str_save_p, *cell_str, *cell_str_save_p;
    uint i, j;
    for (i=0; i<NR; i++) {
        assert((row_str = strtok_r(mat_str, ";", &row_str_save_p)) != NULL);

        // after reading first row, use NULL
        if (mat_str != NULL)
            mat_str = NULL;
        
        // printf("\nrow=\"%s\"\n", row_str);

        for (j=0; j<NC; j++) {
            assert((cell_str = strtok_r(row_str, " ", &cell_str_save_p)) != NULL);

            // after reading first cell, use NULL
            if (row_str != NULL)
                row_str = NULL;
            
            // printf("\ncell=\"%s\"\n", cell_str);
            assert(sscanf(cell_str, "%d", new_mat_p) == 1);
            new_mat_p++;
        }
    }

    free(expr_copy);
    return new_mat;
}

char* infix2postfix_sf(char *infix) {
    char *out = calloc(strlen(infix) + 1, sizeof(char));

    char *stack = calloc(strlen(infix) + 1, sizeof(char));
    uint stack_len = 0;

    char *in_p = infix, *out_p = out, c;
    int true_len = 0;
    while (c = *in_p++) {
        if (isspace(c))
            continue;
        
        switch (c) {
            case '(':
                stack[stack_len++] = c;
                break;
            case ')':
                assert(stack_len > 0);

                // if we see ), we need to dump all ops down to (
                // because we have valid inputs, we assume there is always a ( before a )
                while (stack_len > 0 && stack[stack_len - 1] != '(') {
                    *out_p = stack[stack_len - 1];
                    stack_len--;
                    out_p++;
                    true_len++;
                }

                // now we get rid of the (
                assert(stack_len > 0 && stack[stack_len - 1] == '(');
                stack_len--;

                break;
            case '+':
            case '*':
                // we dump the more or equal precedence ops onto postfix
                while (stack_len > 0 && op_to_precendence(stack[stack_len - 1]) >= op_to_precendence(c)) {
                    *out_p = stack[stack_len - 1];
                    stack_len--;
                    out_p++;
                    true_len++;
                }

                // now stack is either empty, at (, or smaller op
                stack[stack_len++] = c;
                
                break;
            case '\'':
            default:
                *out_p = c;
                out_p++;
                true_len++;
                break;
        }
    }

    // at this point, we are only left with operators and no ()
    while (stack_len > 0) {
        *out_p = stack[stack_len - 1];
        assert(*out_p != '(');
        stack_len--;
        out_p++;
        true_len++;
    }

    free(stack);
    return realloc(out, (true_len + 1)*sizeof(char));
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    return NULL;
}

matrix_sf *execute_script_sf(char *filename) {
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
