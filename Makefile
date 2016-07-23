PROJECT = mysql_poolboy
DEPS = mysql poolboy
dep_mysql = git https://github.com/mysql-otp/mysql-otp.git 1.2.0
dep_poolboy = git https://github.com/devinus/poolboy.git 1.5.1
include erlang.mk
