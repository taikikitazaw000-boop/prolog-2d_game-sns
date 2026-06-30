#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 101

struct member_list {
    char name[64];
    int age;
    struct follow_node *follow_head; 
    struct member_list *next;        
};

struct follow_node {
    struct member_list *user;        
    struct follow_node *next;
};

struct member_list *hashtable[TABLE_SIZE];

// ハッシュ関数
int hash(char *key) {
    unsigned int hashval = 0;
    while (*key != '\0') {
        hashval += *key;
        key++;
    }
    return hashval % TABLE_SIZE;
}

// ユーザー追加
void add(char *name, int age) {
    struct member_list *ptr;
    int x = hash(name);
    ptr = malloc(sizeof(struct member_list));
    if (ptr == NULL) {
        printf("エラー\n");
        return;
    }

    ptr->age = age;
    strcpy(ptr->name, name);
    ptr->follow_head = NULL;

    ptr->next = hashtable[x];
    hashtable[x] = ptr;
}

// 人を探す
struct member_list *find_user(char *name) {
    int index = hash(name);
    struct member_list *current = hashtable[index];

    while (current != NULL) {
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

// フォローする
void follow(char *from, char *to) {
    struct member_list *from_user = find_user(from);
    struct member_list *to_user = find_user(to);

    if (from_user == NULL || to_user == NULL) {
        printf("Error: User not found.\n");
        return;
    }

    struct follow_node *new_follow = malloc(sizeof(struct follow_node));
    if (new_follow == NULL) {
        printf("エラー\n");
        return;
    }

    new_follow->user = to_user;
    new_follow->next = from_user->follow_head;
    from_user->follow_head = new_follow;
    
    printf("Success: %s followed %s.\n", from, to);
}

// フォロー解除
void unfollow(char *from, char *to) {
    struct member_list *from_user = find_user(from);
    struct member_list *to_user = find_user(to);

    if (from_user == NULL || to_user == NULL) {
        printf("Error: User not found.\n");
        return;
    }

    struct follow_node *current = from_user->follow_head;
    struct follow_node *prev = NULL;

    while (current != NULL) {
        if (current->user == to_user) {
            break;
        }
        prev = current;
        current = current->next;
    }

    if (current == NULL) {
        printf("Error: Not following.\n");
        return;
    }

    if (current == from_user->follow_head) {
        from_user->follow_head = current->next;
    } else {
        prev->next = current->next;
    }

    free(current);
    printf("Success: %s unfollowed %s.\n", from, to);
}

// ユーザー情報とフォローリストの表示
void search(char *name) {
    struct member_list *target = find_user(name);

    if (target == NULL) {
        printf("Error: User not found.\n");
        return;
    }

    printf("[User] %s (%d)\n", target->name, target->age);
    printf("Following: ");
    
    struct follow_node *current = target->follow_head;
    while (current != NULL) {
        printf("%s ", current->user->name);
        current = current->next;
    }
    printf("\n");
}

// 相互フォロー判定
void is_mutual(char *name1, char *name2) {
    struct member_list *user1 = find_user(name1);
    struct member_list *user2 = find_user(name2);

    if (user1 == NULL || user2 == NULL) {
        printf("Error: User not found.\n");
        return;
    }

    int follow1_to_2 = 0;
    int follow2_to_1 = 0;

    struct follow_node *current1 = user1->follow_head;
    while (current1 != NULL) {
        if (current1->user == user2) {
            follow1_to_2 = 1;
            break;
        }
        current1 = current1->next;
    }

    struct follow_node *current2 = user2->follow_head;
    while (current2 != NULL) {
        if (current2->user == user1) {
            follow2_to_1 = 1;
            break;
        }
        current2 = current2->next;
    }

    if (follow1_to_2 == 1 && follow2_to_1 == 1) {
        printf("Yes, %s and %s are mutual friends!\n", name1, name2);
    } else {
        printf("No, %s and %s are not mutual friends.\n", name1, name2);
    }
}

// おすすめユーザー推薦
void recommend(char *name) {
    struct member_list *target = find_user(name);
    if (target == NULL) {
        printf("Error: User not found.\n");
        return;
    }
    
    printf("Recommended for %s:\n", name);
    struct follow_node *current_friend = target->follow_head;
    
    while (current_friend != NULL) { 
        struct follow_node *current_fof = current_friend->user->follow_head;
        while (current_fof != NULL) {
            if (current_fof->user != target) {
                printf("- %s (because %s follows them)\n", current_fof->user->name, current_friend->user->name);
            }
            current_fof = current_fof->next;
        }  
        current_friend = current_friend->next;
    }
}

// ファイル読み込み処理
void load_data(char *filename) {
    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
        printf("Error: Cannot open file %s\n", filename);
        return;
    }

    char line[256];
    int mode = 0; // 0: 初期, 1: USERS, 2: FOLLOWS

    while (fgets(line, sizeof(line), fp) != NULL) {
        line[strcspn(line, "\n")] = 0;

        if (line[0] == '#' || line[0] == '\0') {
            if (strstr(line, "# USERS")) mode = 1;
            else if (strstr(line, "# FOLLOWS")) mode = 2;
            continue;
        }
        
        if (mode == 1) { 
            char name[64];
            int age;
            if (sscanf(line, "%s %d", name, &age) == 2) {
                add(name, age);
            }
        } else if (mode == 2) { 
            char from[64], to[64];
            if (sscanf(line, "%s %s", from, to) == 2) {
                // 初期読み込み時は "Success" の表示を消すために静かに繋ぐ
                struct member_list *from_user = find_user(from);
                struct member_list *to_user = find_user(to);
                if (from_user && to_user) {
                    struct follow_node *new_follow = malloc(sizeof(struct follow_node));
                    new_follow->user = to_user;
                    new_follow->next = from_user->follow_head;
                    from_user->follow_head = new_follow;
                }
            }
        }
    }
    fclose(fp);
}

// メニュー表示
void print_menu() {
    printf("--- User Management System ---\n");
    printf("Commands:\n");
    printf("search [name] : Show user info and following list\n");
    printf("add [name] [age] : Register a new user\n");
    printf("follow [from] [to] : Create a follow relationship\n");
    printf("unfollow [from] [to] : Remove a follow relationship\n");
    printf("is_mutual [name1] [name2] : Check mutual follow status\n");
    printf("recommend [name] : Show 'friend of friends'\n");
    printf("exit : Quit program\n");
    printf("-----------------------------\n\n");
}

// main関数
int main() {
    char input[256];
    char cmd[32], arg1[64], arg2[64];

    // 配列の初期化 (安全のため)
    for (int i = 0; i < TABLE_SIZE; i++) {
        hashtable[i] = NULL;
    }

    // 1. データの読み込み
    printf("Loading data from sns_data.txt... ");
    load_data("sns_data.txt");
    printf("Done.\n");

    // 2. メニューの表示
    print_menu();

    // 3. 対話的なコマンド受付ループ
    while (1) {
        printf("command > ");
        
        if (fgets(input, sizeof(input), stdin) == NULL) {
            break; 
        }
        input[strcspn(input, "\n")] = '\0';
        if (strlen(input) == 0) continue; 

        cmd[0] = '\0'; arg1[0] = '\0'; arg2[0] = '\0';
        int num_args = sscanf(input, "%s %s %s", cmd, arg1, arg2);

        if (strcmp(cmd, "exit") == 0) {
            printf("Goodbye!\n");
            break; 
        } 
        else if (strcmp(cmd, "search") == 0 && num_args >= 2) {
            search(arg1);
        } 
        else if (strcmp(cmd, "add") == 0 && num_args >= 3) {
            add(arg1, atoi(arg2));
            printf("Success: Added %s.\n", arg1);
        } 
        else if (strcmp(cmd, "follow") == 0 && num_args >= 3) {
            follow(arg1, arg2);
        } 
        else if (strcmp(cmd, "unfollow") == 0 && num_args >= 3) {
            unfollow(arg1, arg2);
        } 
        else if (strcmp(cmd, "is_mutual") == 0 && num_args >= 3) {
            is_mutual(arg1, arg2);
        } 
        else if (strcmp(cmd, "recommend") == 0 && num_args >= 2) {
            recommend(arg1);
        } 
        else {
            printf("Unknown command or missing arguments.\n");
        }
        
        printf("\n"); 
    }

    return 0;
}