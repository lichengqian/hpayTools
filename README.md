# hpayTools
  包括hpayd 和 hpayctl两个程序
##hpayd
  服务端程序，需要放置在公网服务器，指定端口即可，默认端口为1230
  
##hpayctl
  客户端程序，多命令模式
  *  hpayctl tcpshare -h[HOST:PORT] -p[LOCALHOST:LOCALPORT:REMOTENAME] -l[LOGLEVEL]
     将内网指定的tcp服务端口共享出来，需要指定一个REMOTENAME
  *  hpayctl tcpconnect -h[HOST:PORT] -p[LOCALPORT:REMOTENAME] -l[LOGLEVEL]
     将共享出来的tcp服务端口影射为本机的指定tcp监听端口
     
  
  其中[HOST:PORT]指的是hpayd所在的主机和端口,如果不指定，则需要设置用户变量
    HPAYD=HOST:PORT
  
  
#源码自包含
  程序自包含源码，使用如下命令导出源码：
  hpayd   ----source
  hpayctl ----source
