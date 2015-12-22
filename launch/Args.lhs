




% launch/Args.lhs
% 标准启动器的 选项设置

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
          { ybPort :: Int
          , ybCml  :: Int
          , dbAddr :: String
          , dbPort :: String
          , dbName :: String
          , dbUsr  :: String
          , dbPass :: String
          , isOut  :: Bool
          , isEtc  :: Bool
          } deriving (Show,Typeable,Data)
        launch = Launch
          { ybPort = 3000
            &= help "The port or YrarbilBackend."
            &= name "port"
            &= explicit
            &= groupname "Backend Settings"
          , ybCml = 10
            &= help "The limit of the number of connection between backend and database."
            &= name "cml"
            &= explicit
            &= groupname "Backend Settings"
          , dbAddr = "0.0.0.0"
            &= help "The address of the database."
            &= name "dbaddr"
            &= explicit
            &= groupname "Database Settings"
          , dbPort = "5432"
            &= help "The port for connecting database."
            &= name "dbport"
            &= explicit
            &= groupname "Database Settings"
          , dbName = "postgres"
            &= help "The name of database."
            &= name "dbname"
            &= explicit
            &= groupname "Database Settings"
          , dbUsr = "postgres"
            &= help "The user to connect database."
            &= name "dbusr"
            &= explicit
            &= groupname "Database Settings"
          , dbPass = ""
            &= help "The user's password."
            &= name "dbpw"
            &= explicit
            &= groupname "Database Settings"
          , isOut = False
            &= help "Whether just output configuration file via stdout or just pass it into yb.bin."
            &= name "out"
            &= name "o"
            &= explicit
          , isEtc = False
            &= help "Whether load or write configuration to /etc or some thing like that."
            &= name "config"
            &= name "c"
            &= explicit
          }
          &= details
            [ "This is an \"normal\" launcher of YrarbilBackend. You can use this launch \'whenever\'"
            ]
          &= summary "yb.launch v0.0.8, (C) Qinka 2015"
          &= program "yb.launch"
          &= verbosity
\end{code}

\subsection{执行}
\begin{code}
        runArgs :: (Launch ->IO()) -> IO ()
        runArgs = (cmdArgs launch >>=)
\end{code}
