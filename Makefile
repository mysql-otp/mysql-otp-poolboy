PROJECT = mysql_poolboy
DEPS = mysql poolboy
dep_mysql = git https://github.com/mysql-otp/mysql-otp.git 0.8.0
dep_poolboy = git https://github.com/devinus/poolboy.git 1.4.2
include erlang.mk
