




% 图书借阅管理
% Management.lhs

% 导入导言区
\input{preamble}

\subsection{特性}
\begin{code}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
\end{code}

\subsection{模块 Management}
\begin{code}
module Management
      ( module Management
      , module Management.Data
      ) where
\end{code}

\subsection{导入}
导入 Yesod 与 Management.Data。
\begin{code}
        import Yesod
        import Management.Data
\end{code}
处理JSON。
\begin{code}
        import Data.Aeson
\end{code}
处理 Maybe。
\begin{code}
        import Data.Maybe
\end{code}
对 Text 的支持.
\begin{code}
        import Prelude hiding(words,length,concat,splitAt)
        import Data.Text.Lazy hiding(head,read)
        import Data.Text.Lazy.Encoding(decodeUtf8,encodeUtf8)
\end{code}
Persistent \& PostgreSQL
\begin{code}
        import Database.Persist
        import Database.Persist.TH
        import Database.Persist.Postgresql
\end{code}
处理时间。
\begin{code}
        import Data.Time
\end{code}
\subsection{数据库处理}
使用 Persistent 处理数据库。
\begin{code}
        instance YesodPersist Management where
          type YesodPersistBackend Management = SqlBackend
          runDB a = do
            Auther p <- getYesod
            runSqlPool a p
\end{code}
数据库的表。
\begin{description}
\item[图书信息表] table_bookinfo
\item[图书实体表] table_bookitem
\item[图书操作表] table_bookopt_main
\item[处罚记录表] table_punish
\item[读者数据表] table_reader
\item[操作记录] table_opt
\end{description}
\begin{code}
        share [mkPersist sqlSettings,mkMigrate "migrateAll"] [persistLowerCase|
          Bookinfo json sql=table_bookinfo
            Id sql=
            isbn Int
            name Text Maybe sql=bookname
            author Text Maybe
            plocal Text Maybe sql=publish_local
            phouse Text Maybe sql=publish_house
            pdate Day Maybe sql=publish_date
            llocal Text Maybe sql=library_local
            llindex Text Maybe sql=library_index
            Primary isbn
            deriving Eq Show
          Bookitem json sql=table_bookinfo
            Id sql=
            barcode Int
            isbn Int
            onshelf Bool sql=on_shelf
            there Bool sql=is_there
            lastid Text Maybe sql=latest_opt_id
            bprice Int Maybe sql=bought_price
            bdate Day Maybe sql=bought_date
            Primary barcode
            deriving Eq Show
          Bookopt json sql=table_bookopt_main
            Id sql=
            bsm Text sql=big_serial_number
            rbc Text sql=reader_barcode
            bbc Int sql=book_barcode
            timelmt [Int] sql=time_lmt
            rtdate Day Maybe
            Primary bsm
            deriving Eq Show
          Punish json sql=table_punish
            Id sql=
            bsm Text sql=big_serial_number
            rbc Text sql=reader_barcode
            cash Double sqltype=money
            reason Text
            Primary bsm
            deriving Eq Show
          Reader json sql=table_reader
            Id sql=
            barcode TEXT
            idctp Text Maybe sql=idcard_type
            idcid Text Maybe sql=idcard_id
            debt Double sqltype=money
            epw Text sql=enter_password
            Primary barcode
            deriving Eq Show
          Opt json sql=table_opt
            Id sql=
            ssm Int sql=small_serial_number
            date Day sql=opt_date
            usrtp Int sql=opt_usr_type
            usrid Text sql=opt_usr_id
            Primary ssm
            deriving Eq Show
          |]

\end{code}
