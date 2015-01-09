%% MySQL/OTP + Poolboy
%% Copyright (C) 2015 Raoul Hess
%%
%% This file is part of MySQL/OTP + Poolboy.
%%
%% MySQL/OTP + Poolboy is free software: you can redistribute it and/or modify it under
%% the terms of the GNU Lesser General Public License as published by the Free
%% Software Foundation, either version 3 of the License, or (at your option)
%% any later version.
%%
%% This program is distributed in the hope that it will be useful, but WITHOUT
%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
%% FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for
%% more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program. If not, see <https://www.gnu.org/licenses/>.

-module(mysql_poolboy_sup).

-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Pools = application:get_all_env(mysql_poolboy),
    Pools1 = proplists:delete(included_applications, Pools),
    PoolSpec = lists:map(
        fun ({PoolName, {PoolArgs, MysqlArgs}}) ->
            mysql_poolboy:child_spec(PoolName, PoolArgs, MysqlArgs)
        end,
        Pools1
    ),
    {ok, {{one_for_one, 10, 10}, PoolSpec}}.
