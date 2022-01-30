#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void error() {
  fprintf(stderr, "runtime error\n");
  exit(1);
}

void printString(char *str) {
  if (str == NULL) {
    printf("\n");
  } else {
    printf("%s\n", str);
  }
}

// Returns pointer to beginning of the string, which is packed into the box for
// metadata
char *__read_string() {
  const size_t metadata_padding = 8;
  const size_t d_word = 4;
  size_t buffer_size = 0;
  char *buffer = NULL;

  size_t len_read = getline(&buffer, &buffer_size, stdin);
  if (len_read <= 0) {
    return NULL;
  }

  buffer[len_read - 1] = '\0';
  char *result = calloc(metadata_padding + len_read, d_word);
  memcpy(result + metadata_padding, buffer, len_read);
  free(buffer);

  return result + metadata_padding;
}

void printInt(int x) { printf("%d\n", x); }

char *__read_int_helper() {
  size_t buffer_size = 0;
  char *buffer = NULL;
  size_t len_read = getline(&buffer, &buffer_size, stdin);
  if (len_read <= 0) {
    return NULL;
  }
  buffer[len_read - 1] = '\0';
  return buffer;
}

int readInt() {
  char *input = __read_int_helper();
  int read_value = atoi(input);
  free(input);
  return read_value;
}

// string memory layout:
// [dtr, ref_cnt, c0, c1, ...]
//                ^ pointer to str
char *__strconcat(void *str1, void *str2) {
  const size_t metadata_padding = 8;

  if (str1 == NULL && str2 == NULL) {
    return NULL;
  } else if (str1 == NULL) {
    size_t length = strlen(str2);
    char *result = malloc(length + 1 + metadata_padding);
    memcpy(result + metadata_padding, str2, length + 1);
    return result + metadata_padding;
  } else if (str2 == NULL) {
    size_t length = strlen(str1);
    char *result = malloc(length + 1 + metadata_padding);
    memcpy(result + metadata_padding, str1, length + 1);
    return result + metadata_padding;
  } else {
    size_t length1 = strlen(str1);
    size_t length2 = strlen(str2);

    char *result = malloc(length1 + length2 + 1 + metadata_padding);
    memcpy(result + metadata_padding, str1, length1);
    memcpy(result + metadata_padding + length1, str2,
           length2 + 1); // include end_of_string

    return result + metadata_padding;
  }
}

char *__rstrconcat(void *str1, void *str2) { return __strconcat(str2, str1); }

// In Latte implementation NULL string == empty string
int __streq(const char *str1, const char *str2) {
  const char *empty_str = "";
  if ((str1 == NULL || (strcmp(str1, empty_str) == 0)) &&
      (str2 == NULL || (strcmp(str2, empty_str) == 0))) {
    return 1;
  } else if (str1 == NULL || str2 == NULL) {
    return 0;
  }
  return strcmp(str1, str2) == 0;
}

void __incr_ref_counter(void *memory_loc) {
  if (memory_loc != NULL) {
    int *p = (int *)memory_loc;
    int *ref_counter = p - 1;
    (*ref_counter)++;
  }
}
