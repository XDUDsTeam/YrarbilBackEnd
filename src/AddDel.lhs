




% AddDel.lhs
% 图书的录入与销毁

% 导入导言区
\input{preamble}
\subsection{特性}
\begin{code}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ViewPatterns               #-}
\end{code}
\subsection{模块 AddDel}
\begin{code}
module AddDel
    ( module AddDel
    , module AddDel.Data
    ) where
\end{code}
导入 Yesod 与 AddDel.Del。
\begin{code}
        import Yesod
        import AddDel.Data
\end{code}
使用 aeson 处理 JSON 。
\begin{code}
        import Data.Aeson
\end{code}
对 Maybe 的处理。
\begin{code}
        import Data.Maybe
\end{code}
对 Text 的处理。
\begin{code}
        import Prelude hiding ()
        import Data.Text.Lazy
\end{code}
Persist \& PostgreSQL。
\begin{code}
        import Database.Persist
        import Database.Persist.TH
        import Database.Persist.Postgresql
\end{code}
时间数据。
\begin{code}
        import Data.Time
\end{code}

\subsection{数据处理}
使用 Persistent 处理数据库。
\begin{code}
        instance YesodPersist AddDel where
          type YesodPersistBackend AddDel = SqlBackend
          runDB a = do
            AddDel p <- getYesod
            runSqlPool a p
\end{code}
图书记录的表 table\_bookinfo。
图书实体记录的表 table\_bookitem。
图书购入记录的表 table\_bookopt\_in。
图书销毁记录的表 table\_bookpt\_out。
操作记录的表 table\_opt。
\begin{code}
        share [mkPersist sqlSettings,mkMigrate "migrateAll"] [persistLowerCase|
          Bookinfo json sql=table_bookinfo
            Id sql=
            isbn Int
            bookname Text Maybe
            bookauth Text Maybe sql=author
            publocal Text Maybe sql=publish_local
            pubhouse Text Maybe sql=publish_house
            pubdate Day Maybe sql=publish_date
            liblical Text Maybe sql=library_local
            libindex Text Maybe sql=library_index
            Primary isbn
            deriving Show Eq
          Bookitem json sql=table_bookitem
            Id sql=
            barcode Int
            isbn Int
            shelf Bool sql=on_shelf
            there Bool sql=is_there
            latestopt Text Maybe sql=latest_opt_id
            bdate Day Maybe sql=bought_date
            dprice Double sqltype=money sql=bought_price
            Primary barcode
            deriving Show Eq
          Bookoptin json sql=table_bookopt_in
            Id sql=
            bsm Text sql=big_serial_number
            price Double sql=bought_price sqltype=money
            isbn Int
            barcode Int
            note Text Maybe
            Primary bsm
            deriving Show Eq
          Bookoptout json sql=table_bookopt_out
            Id sql=
            bsm Text sql=big_serial_number
            reason Text
            note Text Maybe
            Primary bsm
            deriving Show Eq
          Opt json sql=table_opt
            Id sql=
            ssm Int sql=small_serial_number
            date  Day sql=opt_date
            utype Int sql=opt_usr_type
            uid Text sql=opt_usr_id
            Primary ssm
            deriving Show Eq 
          |]
\end{code}
