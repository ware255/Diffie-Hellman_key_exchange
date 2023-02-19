# Diffie-Hellman_key_exchange
FortranでDiffie-Hellman鍵交換のプログラムを書いてみました。ぜひ見てね。<br><br>
このプログラムを応用して多倍長整数を実装したりソケット通信プログラムを作るのもありですね。

## 動作環境
* Windows 10以降
* Linux (できるんじゃないですかね？)

## コンパイル
### Windowsの方
build.batを実行するだけです。(gfortranを入れてある前提)
### Linuxの方(Debian系Linux)
```
$ sudo apt update && sudo apt upgrade -y
$ sudo apt install git gfortran
$ git clone https://github.com/ware255/Diffie-Hellman_key_exchange.git
$ cd Diffie-Hellman_key_exchange
$ gfortran main.f90 -o Diffie-Hellman -fbackslash -static -Wall
$ ./Diffie-Hellman
```

## 参考資料
[Diffie Hellman鍵交換解説 図付き](https://youtu.be/XOn3dt0y8iE)
