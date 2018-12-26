//
// Created by andre on 2018-12-26.
//

#include <sys/socket.h>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sysexits.h>
#include <unistd.h>

// 最大同時処理数
#define    MAX_CHILD    (20)
#define    MAX_ONE_LINE (72+1)
#define    MAX_NAME     (16+1)

struct tag_child {
    int fd;
    size_t bufIdx;
    char buf[MAX_ONE_LINE];
    char name[MAX_NAME];
};

static int server_socket(const char *portnm);
static int send_recv(struct tag_child *child, int child_no);
static void accept_loop(int soc);

//
// サーバーソケット準備
//   portnm: 待ち受けるポート番号またはサービスの名前
static int
server_socket(const char *portnm)
{
    char nbuf[NI_MAXHOST], sbuf[NI_MAXSERV];
    struct addrinfo hints, *res0;
    int soc, opt, errcode;
    socklen_t opt_len;

    // アドレス情報のヒントをゼロクリア
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;

    // アドレス情報の決定
    if ((errcode = getaddrinfo(NULL, portnm, &hints, &res0)) != 0) {
        fprintf(stderr, "getaddrinfo():%s\n", gai_strerror(errcode));
        return -1;
    }
    if ((errcode = getnameinfo(res0->ai_addr, res0->ai_addrlen,
                               nbuf, sizeof(nbuf),
                               sbuf, sizeof(sbuf),
                               NI_NUMERICHOST | NI_NUMERICSERV)) != 0) {
        fprintf(stderr, "getnameinfo():%s\n", gai_strerror(errcode));
        freeaddrinfo(res0);
        return -1;
    }
    fprintf(stderr, "port=%s\n", sbuf);

    // ソケットの生成
    if ((soc = socket(res0->ai_family, res0->ai_socktype, res0->ai_protocol)) == -1) {
        perror("socket");
        freeaddrinfo(res0);
        return -1;
    }
    // ソケットオプション（再利用フラグ）設定
    opt = 1;
    opt_len = sizeof(opt);
    if (setsockopt(soc, SOL_SOCKET, SO_REUSEADDR, &opt, opt_len) == -1) {
        perror("setsockopt");
        close(soc);
        freeaddrinfo(res0);
        return -1;
    }
    // ソケットにアドレスを指定
    if (bind(soc, res0->ai_addr, res0->ai_addrlen) == -1) {
        perror("bind");
        close(soc);
        freeaddrinfo(res0);
        return -1;
    }
    // アクセスバックログの指定
    if (listen(soc, SOMAXCONN) == -1) {
        perror("listen");
        close(soc);
        freeaddrinfo(res0);
        return -1;
    }
    freeaddrinfo(res0);

    return soc;
}

//
// 送受信処理
//   *child  : child配列
//   child_no: 受信したchildのインデックス
static int
send_recv(struct tag_child *child, int child_no)
{
    char buf[512];
    ssize_t len;
    int i, sendFlag = 0;

    // 受信
    if ((len = recv(child[child_no].fd, buf, sizeof(buf), 0)) == -1) {
        perror("recv");
        return -1;
    }
    if (len == 0) {
        // 切断
        fprintf(stderr, "[child%d]recv:EOF\n", child_no);
        return -1;
    }

    for (i = 0; i < len && child[child_no].bufIdx < MAX_ONE_LINE - 1 ; i++, child[child_no].bufIdx++) {
        child[child_no].buf[child[child_no].bufIdx] = buf[i];
        if (buf[i] == '\n') {
            if (child[child_no].name[0] == '\0') {
                strncpy(child[child_no].name, child[child_no].buf, sizeof(child[child_no].name) - 1);
                child[child_no].name[child[child_no].bufIdx] = '\0';
                child[child_no].bufIdx = 0;
                memset(child[child_no].buf, '\0', sizeof(child[i].buf));
            } else {
                sendFlag = 1;
            }

            break;
        }
    }

    if (sendFlag) {
        child[child_no].buf[child[child_no].bufIdx] = '\n';
        child[child_no].bufIdx++;
        for (i = 0; i < MAX_CHILD; i++) {
            if (i != child_no && child[i].fd != -1) {
                // 他の child達に送信
                if ((len = send(child[i].fd, child[child_no].name, strlen(child[child_no].name), 0)) == -1) {
                    perror("send");
                }
                if ((len = send(child[i].fd, ": ", 2, 0)) == -1) {
                    perror("send");
                }
                if ((len = send(child[i].fd, child[child_no].buf, child[child_no].bufIdx, 0)) == -1) {
                    perror("send");
                }
            }
        }
        child[child_no].bufIdx = 0;
        memset(child[child_no].buf, '\0', sizeof(child[i].buf));
    }

    return 0;
}

//
// 接続待ちループ
//   soc: 待ち受けソケット
static void
accept_loop(int soc)
{
    char hbuf[NI_MAXHOST], sbuf[NI_MAXSERV];
    struct tag_child child[MAX_CHILD];
    struct timeval timeout;
    struct sockaddr_storage from;
    int acc, child_no, width, i, count, pos, ret;
    socklen_t len;
    fd_set mask;

    // child配列の初期化
    for (i = 0; i < MAX_CHILD; i++) {
        child[i].fd = -1;
        child[i].bufIdx = 0;
        memset(child[i].buf, '\0', sizeof(child[i].buf));
        memset(child[i].name, '\0', sizeof(child[i].name));
    }
    child_no = 0;

    for (;;) {
        // select()用マスクの作成
        FD_ZERO(&mask);
        FD_SET(soc, &mask);
        width = soc + 1;
        count = 0;
        for (i = 0; i < child_no; i++) {
            if (child[i].fd != -1) {
                FD_SET(child[i].fd, &mask);
                if (child[i].fd + 1 > width) {
                    width = child[i].fd + 1;
                    count++;
                }
            }
        }

        // select()用タイムアウト値のセット
        timeout.tv_sec = 10;
        timeout.tv_usec = 0;
        switch (select(width, (fd_set *) &mask, NULL, NULL, &timeout)) {
            case -1:
                // エラー
                perror("select");
                break;
            case 0:
                // タイムアウト
                break;
            default:
                // 着信あり
                if (FD_ISSET(soc, &mask)) {
                    // 接続受付
                    len = (socklen_t) sizeof(from);
                    if ((acc = accept(soc, (struct sockaddr *)&from, &len)) == -1) {
                        if(errno!=EINTR){
                            perror("accept");
                        }
                    } else {
                        getnameinfo((struct sockaddr *) &from, len,
                                    hbuf, sizeof(hbuf),
                                    sbuf, sizeof(sbuf),
                                    NI_NUMERICHOST | NI_NUMERICSERV);
                        fprintf(stderr, "accept:%s:%s\n", hbuf, sbuf);
                        // childの空きを検索
                        pos = -1;
                        for (i = 0; i < child_no; i++) {
                            if (child[i].fd == -1) {
                                pos = i;
                                break;
                            }
                        }
                        if (pos == -1) {
                            // 空きが無い(childにこれ以上格納できない)
                            if (child_no + 1 >= MAX_CHILD) {
                                fprintf(stderr, "child is full : cannot accept\n");
                                // 切断
                                close(acc);
                            } else {
                                child_no++;
                                pos = child_no - 1;
                            }
                        }
                        if (pos != -1) {
                            // childに格納
                            child[pos].fd = acc;
                            fprintf(stderr, "<<child count:%d>>\n", count + 1);
                        }
                        send(child[pos].fd, "Please input your name> ", 24, 0);
                    }
                }

                // child から受信あり
                for (i = 0; i < child_no; i++) {
                    if (child[i].fd != -1) {
                        if (FD_ISSET(child[i].fd, &mask)) {
                            // 送受信処理
                            if ((ret = send_recv(child, i)) == -1) {
                                // エラーまたは切断
                                close(child[i].fd);
                                // childを空きに更新
                                child[i].fd = -1;
                                child[i].bufIdx = 0;
                                memset(child[i].buf, '\0', sizeof(child[i].buf));
                                memset(child[i].name, '\0', sizeof(child[i].name));
                            }
                        }
                    }
                }
                break;
        }
    }
}

int
main(int argc, char *argv[])
{
    int soc;
    char *port;

    // 引数にポート番号が指定されているか？
    if (argc <= 1) {
        port = "20023";
    } else {
        port = argv[1];
    }

    // サーバソケットの準備
    if ((soc = server_socket(port)) == -1) {
        fprintf(stderr, "server_socket(%s):error\n", argv[1]);
        return EX_UNAVAILABLE;
    }
    fprintf(stderr, "ready for accept\n");

    // 送受信ループ
    accept_loop(soc);

    // ソケットクローズ
    close(soc);

    return EX_OK;
}
