




% Appointment.lhs
% 图书预约

%导入导言区
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
{-# LANGUAGE DeriveGeneric              #-}
\end{code}

\subsection{模块 Appointment}
\begin{code}
module Appointment
      ( module Appointment
      , module Appointment.Data
      ) where
\end{code}
\subsection{导入}
导入 Yesod Common Data。
\begin{code}
        import Yesod
        import 
\end{code}