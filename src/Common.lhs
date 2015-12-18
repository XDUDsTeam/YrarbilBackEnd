




% 公共常用函数
% Common.lhs

%导入导言区
\input{preamble}

提供公共函数的模块
\begin{code}
module Common
    ( returnTJson
    , t2t
    , t2it
    , getRandom
    ) where
\end{code}
\subsection{导入}
Aeson 与 Yesod。
\begin{code}
        import Data.Aeson
        import Yesod
\end{code}
对 Text 的支持.
\begin{code}
        import Prelude hiding(words,length,concat,splitAt)
        import Data.Text.Lazy hiding(head,read)
        import Data.Text.Lazy.Encoding(decodeUtf8,encodeUtf8)
        import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
        import Data.Text.Internal (showText)
        import qualified Data.Text.Internal as DTI(showText,Text)
        import Data.Text.Internal.Encoding.Utf8()
        import qualified Data.String as DS
        import Data.String
\end{code}
随机数。
\begin{code}
        import System.Random
\end{code}
返回 Text形式的的JSON 数据的函数。
\begin{code}
        returnTJson :: Monad m =>  Value -> HandlerT site m Text
        returnTJson = return.decodeUtf8.encode
\end{code}

Data.Text.Internal.Text => Data.Lazy.Text
\begin{code}
        t2t :: DTI.Text -> Text
        t2t = fromString.showText
\end{code}
Data.Lazy.Text => Data.Text.Internal.Text
\begin{code}
        t2it :: Text -> DTI.Text
        t2it = fromString.read.show.toStrict
\end{code}
获取随机数。
\begin{code}
        getRandom :: IO Int
        getRandom = getStdRandom random >>= return . abs
\end{code}
