MySQL/OTP - poolboy
=================

MySQL/OTP - poolboy is a wrapper for MySQL/OTP and poolboy where you create pools for MySQL/OTP.
Each application is responsible to supervise the pools by themself, MySQL/OTP - poolboy will not handle
it.

Features:

* Simple pool creation
* Convenience function to do querys and transactions

See also:

* [MySQL/OTP](//github.com/mysql-otp/mysql-otp)
* [Poolboy](//github.com/devinus/poolboy)

Examples:
```Erlang
%% Create a pool with 5 connection and allow another 5 if they are none available.
%% returns a supvervise:child_spec()
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
license texts are included in the files [COPYING](COPYING) and
[COPYING.LESSER](COPYING.LESSER) respectively.

We hope this license should be permissive enough while remaining copyleft. If
you're having issues with this license, please create an issue in the issue
tracker!
