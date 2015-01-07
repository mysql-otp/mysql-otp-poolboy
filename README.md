MySQL/OTP + Poolboy
===================

Status: Not complete and is work in progress.

MySQL/OTP + Poolboy is a wrapper for [MySQL/OTP](//github.com/mysql-otp/mysql-otp) and
[Poolboy](//github.com/devinus/poolboy) where you create connection pools for the former.
Each application is itself responsible for supervising the pools, i.e. MySQL/OTP + Poolboy won't
do it for you.

Features:

* Simple pool creation
* Convenience function to do queries and transactions

Examples:
```Erlang
%% Creates a pool with 5 connections and allows another 5 if there are none available.
%% Returns a supvervise:child_spec()
mysql_poolboy:add_pool(
    {mypool, [{size, 5}, {max_overflow, 10}],
     [{host, "localhost"}, {user, "foo"}, {password, "hello"}, {database, "test"}]}
).

%% Without using the convenience functions in mysql_poolboy.
Conn = mysql_poolboy:checkout_connection(mypool).
Result = mysql:query(Conn, "SELECT * FROM test_table").
%% Should always return the connection when done.
mysql_poolboy:checkin_connection(mypool, Conn).

%% Using mysql_poolboy:query/2
Result1 = mysql_poolboy:query(mypool, "SELECT * FROM test_table").

```

License
-------

GNU Lesser General Public License (LGPL) version 3 or any later version.
Since the LGPL is a set of additional permissions on top of the GPL, both
license texts are included in the files [COPYING.LESSER](COPYING.LESSER) and
[COPYING](COPYING) respectively.
