//
// Created by andre on 2018-12-26.
//

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/types.h>

#include <arpa/inet.h>
#include <arpa/telnet.h>
#include <netinet/in.h>
#include <netdb.h>

#include <ctype.h>
#include <errno.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sysexits.h>
#include <unistd.h>
#include <termios.h>

// ソケット
static int g_soc = -1;

// 終了フラグ
static int  g_end = 0;

static int client_socket(const char *hostnm, const char *portnm);
static void send_recv_loop(void);

//
// サーバーソケットに接続する関数
//   hostnm: ホスト名またはIPアドレスの文字列
//   postnm: ポート番号またはサービス名の文字列
static int
client_socket(const char *hostnm, const char *portnm)
{
    char nbuf[NI_MAXHOST], sbuf[NI_MAXSERV];
    struct addrinfo hints, *res0;
    int soc, errcode;

    // アドレス情報のヒントをゼロクリア
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    // アドレス情報の決定
    if ((errcode = getaddrinfo(hostnm, portnm, &hints, &res0)) != 0) {
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
    fprintf(stderr, "addr=%s\n", nbuf);
    fprintf(stderr, "port=%s\n", sbuf);

    // ソケットの生成
    if ((soc = socket(res0->ai_family, res0->ai_socktype, res0->ai_protocol)) == -1) {
        perror("socket");
        freeaddrinfo(res0);
        return -1;
    }

    // 接続
    if (connect(soc, res0->ai_addr, res0->ai_addrlen) == -1) {
        perror("connect");
        close(soc);
        freeaddrinfo(res0);
        return -1;
    }
    freeaddrinfo(res0);

    return soc;
}

//
// 送受信処理
static void
send_recv_loop(void)
{
    struct pollfd targets[2];
    char c;

    // バッファリングOFF
    setbuf(stdin, NULL);
    setbuf(stdout, NULL);

    // poll()用データの作成
    targets[0].fd = g_soc;
    targets[0].events = POLLIN;
    targets[1].fd = 0;
    targets[1].events = POLLIN;
    for (;;) {
        switch (poll(targets, sizeof targets / sizeof targets[0], 1 * 1000)) {
            case -1:
                if (errno != EINTR) {
                    perror("poll");
                    g_end = 1;
                }
                break;
            case 0:
                // タイムアウト
                break;
            default:
                // 受信データあり
                if (targets[0].revents & (POLLIN | POLLERR)) {
                    // ソケットから受信
                    if (recv(g_soc, &c, 1, 0) <= 0) {
                        g_end = 1;
                        break;
                    }
                    // 画面に表示
                    fputc(c, stdout);
                }
                // キーボードからの入力あり
                if (targets[1].revents & (POLLIN | POLLERR)) {
                    c = getchar();
                    // サーバーに送信
                    if (send(g_soc, &c, 1, 0) == -1) {
                        perror("send");
                        g_end = 1;
                        break;
                    }
                }
                break;
        }
        // g_endフラグが0以外ならループを終了させる
        if (g_end) {
            break;
        }
    }
}

int
main(int argc,char *argv[])
{
    struct termios term, default_term;
    char *port;

    if (argc <= 1) {
        fprintf(stderr, "client hostname [port]\n");
        return EX_USAGE;
    } else if (argc <= 2) {
        port = "20023";
    } else {
        port = argv[2];
    }

    // 端末を非カノニカルモード(1文字単位で入力できるよう)にする
    tcgetattr(fileno(stdin), &term);
    default_term = term;
    term.c_lflag &= ~ICANON;
    tcsetattr(fileno(stdin), TCSANOW, &term);

    // ソケット接続
    if ((g_soc = client_socket(argv[1], port)) == -1) {
        return EX_IOERR;
    }

    // 送受信処理
    send_recv_loop();

    fprintf(stderr, "Connection Closed.\n");
    close(g_soc);

    // 端末設定を元に戻す
    tcsetattr(fileno(stdin), TCSANOW, &default_term);

    return EX_OK;
}
