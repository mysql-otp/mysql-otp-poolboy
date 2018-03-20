MySQL/OTP + Poolboy
===================

Status: Beta.

**MySQL/OTP + Poolboy** provides connection pooling for
[MySQL/OTP](//github.com/mysql-otp/mysql-otp) using
[Poolboy](//github.com/devinus/poolboy). It contains convenience functions for
executing SQL queries on a connection in a pool and lets you choose between two
methods for creating and managing connection pools:

1. Use it as a library that helps you supervise your own MySQL connection pools.
2. Use it as an application that manages its own supervisor for connection pools.

I want to supervise my own connection pools
-------------------------------------------

Use `mysql_poolboy:child_spec/3` to get a child spec for a pool that you can use
in your own supervisor.

```Erlang
%% my own supervisor
init([]) ->
    PoolOptions  = [{size, 10}, {max_overflow, 20}],
    MySqlOptions = [{user, "aladdin"}, {password, "sesame"}, {database, "test"},
                    {prepare, [{foo, "SELECT * FROM foo WHERE id=?"}]}],
    ChildSpecs = [
        %% MySQL pools
        mysql_poolboy:child_spec(pool1, PoolOptions, MySqlOptions),
        %% other workers...
        {some_other_worker, {some_other_worker, start_link, []},
         permanent, 10, worker, [some_other_worker]}
    ],
    {ok, {{one_for_one, 10, 10}, ChildSpecs}}.
```

Let MySQL/OTP + Poolboy supervise my pools
------------------------------------------

This approach requires you to start the application `mysql_poolboy`. Typically
this is done by adding `{applications, [mysql_poolboy]}` to your `.app.src`
file and then relying on your favourite release tool for the rest.

Pools can be added at run-time using `mysql_poolboy:add_pool/3`.

Pools can also be created at start-up by defining configuration parameters for
`mysql_poolboy`. The name of each configuration parameter is the pool name and
the value is a pair on the form `{PoolOptions, MySqlOptions}`.

Example:

Start your Erlang node with `erl -config mypools.config` where `mypools.config`
is a file with the following contents:

```Erlang
[
 {mysql_poolboy, [
    {pool1, {[{size, 10}, {max_overflow, 20}],
             [{user, "aladdin"}, {password, "sesame"}, {database, "test"},
              {prepare, [{foo, "SELECT * FROM foo WHERE id=?"}]}]}}
]}].
```

Using the connection pools
--------------------------

The most commonly used MySQL functions are available with wrappers in
`mysql_poolboy`.

```Erlang
1> mysql_poolboy:query(pool1, "SELECT * FROM foo WHERE id=?", [42]).
{ok,[<<"id">>,<<"bar">>],[[42,<<"baz">>]]}
2> mysql_poolboy:execute(pool1, foo, [42]).
{ok,[<<"id">>,<<"bar">>],[[42,<<"baz">>]]}
```

For transactions, the connection pid is passed to the transaction fun as the
first parameter.

```Erlang
3> mysql_poolboy:transaction(pool1, fun (Pid) ->
       ok = mysql:query(Pid, "INSERT INTO foo VALUES (?, ?)", [1, <<"banana">>]),
       ok = mysql:query(Pid, "INSERT INTO foo VALUES (?, ?)", [2, <<"kiwi">>]),
       hello
   end).
{atomic, hello}
```

Sometimes you need to checkout a connection to execute multiple queries on it,
without wrapping it in an SQL transaction. For this purpose you can use either
a pair of calls to `checkout/1` and `checkin/2` or a call to `with/2` with a
fun as in this example:

```Erlang
4> mysql_poolboy:with(pool1, fun (Pid) ->
       {ok, _, [[OldTz]]} = mysql:query(Pid, "SELECT @@time_zone"),
       ok = mysql:query(Pid, "SET time_zone = '+00:00'"),
       %% Do some stuff in the UTC time zone...
       ok = mysql:query(Pid, "SET time_zone = ?", [OldTz])
   end).
ok
```

Use this as a dependency
------------------------

Using *erlang.mk*, put this in your `Makefile`:

```Erlang
DEPS = mysql_poolboy
dep_mysql_poolboy = git https://github.com/mysql-otp/mysql-otp-poolboy 0.1.8
```

Using *rebar*, put this in your `rebar.config`:

```Erlang
{deps, [
    {mysql_poolboy, ".*", {git, "https://github.com/mysql-otp/mysql-otp-poolboy",
                           {tag, "0.1.8"}}}
]}.
```

License
-------

GNU Lesser General Public License (LGPL) version 3 or any later version.
Since the LGPL is a set of additional permissions on top of the GPL, both
license texts are included in the files [COPYING.LESSER](COPYING.LESSER) and
[COPYING](COPYING) respectively.
