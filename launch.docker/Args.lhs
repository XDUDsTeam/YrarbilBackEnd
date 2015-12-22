




% launch.docker/Args
% 处理 启动器的参数传递。

% 导入导言区
\input{../src/preamble}

\subsection{特性}
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
\end{code}

\subsection{模块 Args}
\begin{code}
module Args
      ( Launch(..)
      , runArgs
      ) where
\end{code}
\subsection{导入}
导入CmdArgs
\begin{code}
        import System.Console.CmdArgs
\end{code}
处理数据

\begin{code}
        import Yrarbil.Backend.Config
\end{code}
\subsection{命令行参数}
\begin{code}
        data Launch = Launch
          { yrarbil_backend_port :: String
          , yrarbil_backend_conlmt :: String
          , postgresql_addr :: String
          , postgresql_port :: String
          , postgresql_db :: String
          , postgresql_pw :: String
          , postgresql_usr :: String
          } deriving (Show, Typeable, Data)
        launch = Launch
          { yrarbil_backend_port = "YRARBIL_BACKEND_PORT"
              &= help "The environment value of backend's port."
              &= name "port"
              &= explicit
              &= groupname "Backend Settings"
          , yrarbil_backend_conlmt = "YRARBIL_BACKEND_CONNECTIONLIMIT"
              &= help "The environment value of the count of connection between backend and datebase."
              &= name "conlmt"
              &= explicit
              &= groupname "Backend Settings"
          , postgresql_addr = "POSTGRESQL_PORT_5432_TCP_ADDR"
              &= help "The environment value of datebase's address."
              &= name "dbaddr"
              &= explicit
              &= groupname "Database Connection Settings"
          , postgresql_db = "POSTGRESQL_INSTANCE_NAME"
              &= help "The environment value of the datebase's name."
              &= name "dbname"
              &= explicit
              &= groupname "Database Connection Settings"
          , postgresql_port = "POSTGRESQL_PORT_5432_TCP_PORT"
              &= help "The environment value of the datebase's port."
              &= name "dbport"
              &= explicit
              &= groupname "Database Connection Settings"
          , postgresql_pw = "POSTGRESQL_PASSWORD"
              &= help "The environment value of the datebase's password."
              &= name "dbpw"
              &= explicit
              &= groupname "Database Connection Settings"
          , postgresql_usr = "POSTGRESQL_USERNAME"
              &= help "The environment value of the datebase's user-name."
              &= name "dbusr"
              &= explicit
              &= groupname "Database Connection Settings"
          }
          &= details
            [ "The is a launcher of yrarbil's backend, which will be used in daocloud's docker-ship."
            ]
          &= summary "yb.docker.launch v0.0.8, (C) Qinka 2015"
          &= program "yb.docker.launch"
          &= verbosity
\end{code}
\subsection{执行}
\begin{code}
        runArgs :: (Launch -> IO Config) -> IO Config
        runArgs = (cmdArgs launch >>=)
\end{code}
