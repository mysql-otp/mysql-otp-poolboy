%% MySQL/OTP - Poolboy handler
%% Copyright (C) 2014 Raoul Hess
%%
%% This file is part of MySQL/OTP - Poolboy handler.
%%
%% MySQL/OTP is free software: you can redistribute it and/or modify it under
%% the terms of the GNU Lesser General Public License as published by the Free
%% Software Foundation, either version 3 of the License, or (at your option)
%% any later version.
%%
%% This program is distributed in the hope that it will be useful, but WITHOUT
%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
%% FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
%% more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program. If not, see <https://www.gnu.org/licenses/>.

-module(mysql_poolboy).

-export([add_pools/1, add_pool/1, checkout_connection/1, checkin_connection/2,
         query/2, query/3, query/4, transaction/2, transaction/3]).

%% @doc Creates pools from a list. Returning a list of supervisor:child_spec().
%%      Each application should supervise these specification themself if needed.
add_pools(Pools) ->
    lists:map(fun (PoolData) -> add_pool(PoolData) end, Pools).

%% @doc Creates a pool with the given arguments returning a supervisor:child_spec()
%%      Each application should supervise these specification themself if needed.
add_pool({PoolName, PoolSizeArgs, MysqlArgs}) ->
    PoolArgs = [{name, {local, PoolName}}, {worker_module, mysql}] ++ PoolSizeArgs,
    poolboy:child_spec(PoolName, PoolArgs, MysqlArgs).

%% Shorthand/convenience functions.

%% @doc Checkouts a mysql connection from a given pool.
checkout_connection(PoolName) ->
    poolboy:checkout(PoolName).

%% @doc Return a mysql connection to a pool when done with it.
checkin_connection(PoolName, Connection) ->
    poolboy:checkin(PoolName, Connection).

%% @doc Execute a query to a mysql connection in a given pool.
query(PoolName, Query) ->
    poolboy:transaction(PoolName, fun(MysqlConn) ->
        mysql:query(MysqlConn, Query)
    end).

%% @doc Execute a query to a mysql connection in a given pool.
query(PoolName, Query, ParamsOrTimeout) ->
    poolboy:transaction(PoolName, fun(MysqlConn) ->
        mysql:query(MysqlConn, Query, ParamsOrTimeout)
    end).

%% @doc Execute a query to a mysql connection in a given pool.
query(PoolName, Query, Params, Timeout) ->
    poolboy:transaction(PoolName, fun(MysqlConn) ->
        mysql:query(MysqlConn, Query, Params, Timeout)
    end).

%% @doc Execute a transaction fun. A connection will be supplied by the pool.
transaction(PoolName, TransactionFun) ->
    poolboy:transaction(PoolName, fun(MysqlConn) ->
        mysql:transaction(MysqlConn, TransactionFun)
    end).

%% @doc Execute a transaction fun. A connection will be supplied by the pool.
transaction(PoolName, TransactionFun, Args) ->
    poolboy:transaction(PoolName, fun(MysqlConn) ->
        mysql:transaction(MysqlConn, TransactionFun, Args)
    end).
