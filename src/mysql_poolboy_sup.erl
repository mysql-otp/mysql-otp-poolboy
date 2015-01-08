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
-export([init/1]).

init([]) ->
    Pools = application:get_all_env(mysql_poolboy),
    PoolSpec = lists:map(
        fun ({PoolName, {PoolArgs, MysqlArgs}}) ->
            mysqp_poolboy:child_spec(PoolName, PoolArgs, MysqlArgs)
        end,
        Pools
    ),
    {ok, {{one_for_one, 10, 10}, PoolSpec}}.
