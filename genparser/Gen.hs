

-- UUAGC 0.9.42.3 (Gen.ag)
module Gen where

{-# LINE 3 "./Gen.ag" #-}

import Data.List (groupBy, intercalate)
import Data.Char (toUpper)
import CCO.Component (Component, component)
{-# LINE 12 "Gen.hs" #-}
{-# LINE 63 "./Gen.ag" #-}

validate :: Component Grammar Document
validate = component (\grammar -> do 
                let wrapper = (wrap_Root (sem_Root $ Root grammar) Inh_Root{})
                let errors  = errs_Syn_Root wrapper
                let prods   = prods_Syn_Root wrapper
                let parsers = map (\(p@(Prod nt _ _):pds) -> Parser nt (p:pds)) $ groupBy (\(Prod n1 _ _) (Prod n2 _ _) -> n1 == n2) prods
                let root    = (\(Parser (NT r) _) -> r) $ head parsers
                case errors of
                    []  -> return $ Document (docHeader root) parsers
                    _   -> fail (unlines errors)
            )

generate :: Component Document String
generate = component (\doc -> return $ show doc)

consParser :: String -> Nodes -> String
consParser c fs = consName ++ app ++ fieldParser fs
    where   consName = map toUpper c
            app      = case head fs of
                        (T _) -> " <$ "
                        _     -> " <$> " 

fieldParser :: Nodes -> String
fieldParser []      = ""
fieldParser [f]     = show f 
fieldParser (f:fs)  = show f ++ conc (head fs) ++ fieldParser fs
    where conc (T x)= " <* "
          conc _    = " <*> "

docHeader r =  unlines  [ "module Parser where"
                        , "import Lexer"
                        , "import CCO.Component as C"
                        , "import CCO.Parsing (Parser(..))"
                        , "import Control.Applicative"
                        , "parser = C.parser lexer p" ++ r
                        ]

instance Show Document where
    show (Document head parsers) = head ++ "\n" ++ unlines parseLines
        where parseLines = 
                plns_Syn_GenParsers $ 
                    wrap_GenParsers (sem_GenParsers parsers) Inh_GenParsers{}

instance Show Node where
    show (NT nt)    = "p" ++ map toUpper nt
    show (T tm)     = "pTm \"" ++ tm ++ "\""
    show (BoolLit)  = "pBool"
    show (IntLit)   = "pInt"
{-# LINE 63 "Gen.hs" #-}
-- Document ----------------------------------------------------
data Document = Document (String) (GenParsers)
-- cata
sem_Document :: Document ->
                T_Document
sem_Document (Document _head _parsers) =
    (sem_Document_Document _head (sem_GenParsers _parsers))
-- semantic domain
type T_Document = ( Document)
data Inh_Document = Inh_Document {}
data Syn_Document = Syn_Document {self_Syn_Document :: Document}
wrap_Document :: T_Document ->
                 Inh_Document ->
                 Syn_Document
wrap_Document sem (Inh_Document) =
    (let ( _lhsOself) = sem
     in  (Syn_Document _lhsOself))
sem_Document_Document :: String ->
                         T_GenParsers ->
                         T_Document
sem_Document_Document head_ parsers_ =
    (let _lhsOself :: Document
         _parsersIplns :: ([String])
         _parsersIself :: GenParsers
         _self =
             Document head_ _parsersIself
         _lhsOself =
             _self
         ( _parsersIplns,_parsersIself) =
             parsers_
     in  ( _lhsOself))
-- GenParser ---------------------------------------------------
data GenParser = Parser (Node) (Prods)
-- cata
sem_GenParser :: GenParser ->
                 T_GenParser
sem_GenParser (Parser _nt _ps) =
    (sem_GenParser_Parser (sem_Node _nt) (sem_Prods _ps))
-- semantic domain
type T_GenParser = ( String,GenParser)
data Inh_GenParser = Inh_GenParser {}
data Syn_GenParser = Syn_GenParser {p_Syn_GenParser :: String,self_Syn_GenParser :: GenParser}
wrap_GenParser :: T_GenParser ->
                  Inh_GenParser ->
                  Syn_GenParser
wrap_GenParser sem (Inh_GenParser) =
    (let ( _lhsOp,_lhsOself) = sem
     in  (Syn_GenParser _lhsOp _lhsOself))
sem_GenParser_Parser :: T_Node ->
                        T_Prods ->
                        T_GenParser
sem_GenParser_Parser nt_ ps_ =
    (let _lhsOself :: GenParser
         _lhsOp :: String
         _ntIid :: String
         _ntIself :: Node
         _psIplns :: ([String])
         _psIself :: Prods
         _p =
             ({-# LINE 57 "./Gen.ag" #-}
              show _ntIself ++ " = " ++ _pCs
              {-# LINE 125 "Gen.hs" #-}
              )
         _pCs =
             ({-# LINE 58 "./Gen.ag" #-}
              intercalate " <|> " _psIplns
              {-# LINE 130 "Gen.hs" #-}
              )
         _self =
             Parser _ntIself _psIself
         _lhsOself =
             _self
         _lhsOp =
             ({-# LINE 53 "./Gen.ag" #-}
              _p
              {-# LINE 139 "Gen.hs" #-}
              )
         ( _ntIid,_ntIself) =
             nt_
         ( _psIplns,_psIself) =
             ps_
     in  ( _lhsOp,_lhsOself))
-- GenParsers --------------------------------------------------
type GenParsers = [GenParser]
-- cata
sem_GenParsers :: GenParsers ->
                  T_GenParsers
sem_GenParsers list =
    (Prelude.foldr sem_GenParsers_Cons sem_GenParsers_Nil (Prelude.map sem_GenParser list))
-- semantic domain
type T_GenParsers = ( ([String]),GenParsers)
data Inh_GenParsers = Inh_GenParsers {}
data Syn_GenParsers = Syn_GenParsers {plns_Syn_GenParsers :: ([String]),self_Syn_GenParsers :: GenParsers}
wrap_GenParsers :: T_GenParsers ->
                   Inh_GenParsers ->
                   Syn_GenParsers
wrap_GenParsers sem (Inh_GenParsers) =
    (let ( _lhsOplns,_lhsOself) = sem
     in  (Syn_GenParsers _lhsOplns _lhsOself))
sem_GenParsers_Cons :: T_GenParser ->
                       T_GenParsers ->
                       T_GenParsers
sem_GenParsers_Cons hd_ tl_ =
    (let _lhsOplns :: ([String])
         _lhsOself :: GenParsers
         _hdIp :: String
         _hdIself :: GenParser
         _tlIplns :: ([String])
         _tlIself :: GenParsers
         _lhsOplns =
             ({-# LINE 55 "./Gen.ag" #-}
              _hdIp : _tlIplns
              {-# LINE 176 "Gen.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIp,_hdIself) =
             hd_
         ( _tlIplns,_tlIself) =
             tl_
     in  ( _lhsOplns,_lhsOself))
sem_GenParsers_Nil :: T_GenParsers
sem_GenParsers_Nil =
    (let _lhsOplns :: ([String])
         _lhsOself :: GenParsers
         _lhsOplns =
             ({-# LINE 51 "./Gen.ag" #-}
              []
              {-# LINE 194 "Gen.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOplns,_lhsOself))
-- Grammar -----------------------------------------------------
data Grammar = Grammar (Productions)
             deriving ( Show)
-- cata
sem_Grammar :: Grammar ->
               T_Grammar
sem_Grammar (Grammar _ps) =
    (sem_Grammar_Grammar (sem_Productions _ps))
-- semantic domain
type T_Grammar = ( ([String]),Prods,Grammar)
data Inh_Grammar = Inh_Grammar {}
data Syn_Grammar = Syn_Grammar {errs_Syn_Grammar :: ([String]),prods_Syn_Grammar :: Prods,self_Syn_Grammar :: Grammar}
wrap_Grammar :: T_Grammar ->
                Inh_Grammar ->
                Syn_Grammar
wrap_Grammar sem (Inh_Grammar) =
    (let ( _lhsOerrs,_lhsOprods,_lhsOself) = sem
     in  (Syn_Grammar _lhsOerrs _lhsOprods _lhsOself))
sem_Grammar_Grammar :: T_Productions ->
                       T_Grammar
sem_Grammar_Grammar ps_ =
    (let _lhsOprods :: Prods
         _lhsOerrs :: ([String])
         _lhsOself :: Grammar
         _psIerrs :: ([String])
         _psIprods :: Prods
         _psIself :: Productions
         _lhsOprods =
             ({-# LINE 34 "./Gen.ag" #-}
              _psIprods
              {-# LINE 231 "Gen.hs" #-}
              )
         _lhsOerrs =
             ({-# LINE 28 "./Gen.ag" #-}
              _psIerrs
              {-# LINE 236 "Gen.hs" #-}
              )
         _self =
             Grammar _psIself
         _lhsOself =
             _self
         ( _psIerrs,_psIprods,_psIself) =
             ps_
     in  ( _lhsOerrs,_lhsOprods,_lhsOself))
-- Node --------------------------------------------------------
data Node = NT (String)
          | T (String)
          | BoolLit
          | IntLit
          deriving ( Eq)
-- cata
sem_Node :: Node ->
            T_Node
sem_Node (NT _ident) =
    (sem_Node_NT _ident)
sem_Node (T _ident) =
    (sem_Node_T _ident)
sem_Node (BoolLit) =
    (sem_Node_BoolLit)
sem_Node (IntLit) =
    (sem_Node_IntLit)
-- semantic domain
type T_Node = ( String,Node)
data Inh_Node = Inh_Node {}
data Syn_Node = Syn_Node {id_Syn_Node :: String,self_Syn_Node :: Node}
wrap_Node :: T_Node ->
             Inh_Node ->
             Syn_Node
wrap_Node sem (Inh_Node) =
    (let ( _lhsOid,_lhsOself) = sem
     in  (Syn_Node _lhsOid _lhsOself))
sem_Node_NT :: String ->
               T_Node
sem_Node_NT ident_ =
    (let _lhsOid :: String
         _lhsOself :: Node
         _lhsOid =
             ({-# LINE 45 "./Gen.ag" #-}
              ident_
              {-# LINE 280 "Gen.hs" #-}
              )
         _self =
             NT ident_
         _lhsOself =
             _self
     in  ( _lhsOid,_lhsOself))
sem_Node_T :: String ->
              T_Node
sem_Node_T ident_ =
    (let _lhsOid :: String
         _lhsOself :: Node
         _lhsOid =
             ({-# LINE 46 "./Gen.ag" #-}
              ident_
              {-# LINE 295 "Gen.hs" #-}
              )
         _self =
             T ident_
         _lhsOself =
             _self
     in  ( _lhsOid,_lhsOself))
sem_Node_BoolLit :: T_Node
sem_Node_BoolLit =
    (let _lhsOid :: String
         _lhsOself :: Node
         _lhsOid =
             ({-# LINE 47 "./Gen.ag" #-}
              "Bool"
              {-# LINE 309 "Gen.hs" #-}
              )
         _self =
             BoolLit
         _lhsOself =
             _self
     in  ( _lhsOid,_lhsOself))
sem_Node_IntLit :: T_Node
sem_Node_IntLit =
    (let _lhsOid :: String
         _lhsOself :: Node
         _lhsOid =
             ({-# LINE 48 "./Gen.ag" #-}
              "Int"
              {-# LINE 323 "Gen.hs" #-}
              )
         _self =
             IntLit
         _lhsOself =
             _self
     in  ( _lhsOid,_lhsOself))
-- Nodes -------------------------------------------------------
type Nodes = [Node]
-- cata
sem_Nodes :: Nodes ->
             T_Nodes
sem_Nodes list =
    (Prelude.foldr sem_Nodes_Cons sem_Nodes_Nil (Prelude.map sem_Node list))
-- semantic domain
type T_Nodes = ( Nodes)
data Inh_Nodes = Inh_Nodes {}
data Syn_Nodes = Syn_Nodes {self_Syn_Nodes :: Nodes}
wrap_Nodes :: T_Nodes ->
              Inh_Nodes ->
              Syn_Nodes
wrap_Nodes sem (Inh_Nodes) =
    (let ( _lhsOself) = sem
     in  (Syn_Nodes _lhsOself))
sem_Nodes_Cons :: T_Node ->
                  T_Nodes ->
                  T_Nodes
sem_Nodes_Cons hd_ tl_ =
    (let _lhsOself :: Nodes
         _hdIid :: String
         _hdIself :: Node
         _tlIself :: Nodes
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIid,_hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Nodes_Nil :: T_Nodes
sem_Nodes_Nil =
    (let _lhsOself :: Nodes
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Prod --------------------------------------------------------
data Prod = Prod (Node) (String) (Nodes)
-- cata
sem_Prod :: Prod ->
            T_Prod
sem_Prod (Prod _nt _cs _fs) =
    (sem_Prod_Prod (sem_Node _nt) _cs (sem_Nodes _fs))
-- semantic domain
type T_Prod = ( String,Prod)
data Inh_Prod = Inh_Prod {}
data Syn_Prod = Syn_Prod {p_Syn_Prod :: String,self_Syn_Prod :: Prod}
wrap_Prod :: T_Prod ->
             Inh_Prod ->
             Syn_Prod
wrap_Prod sem (Inh_Prod) =
    (let ( _lhsOp,_lhsOself) = sem
     in  (Syn_Prod _lhsOp _lhsOself))
sem_Prod_Prod :: T_Node ->
                 String ->
                 T_Nodes ->
                 T_Prod
sem_Prod_Prod nt_ cs_ fs_ =
    (let _lhsOp :: String
         _lhsOself :: Prod
         _ntIid :: String
         _ntIself :: Node
         _fsIself :: Nodes
         _lhsOp =
             ({-# LINE 61 "./Gen.ag" #-}
              "(" ++ consParser cs_ _fsIself ++ ")"
              {-# LINE 402 "Gen.hs" #-}
              )
         _self =
             Prod _ntIself cs_ _fsIself
         _lhsOself =
             _self
         ( _ntIid,_ntIself) =
             nt_
         ( _fsIself) =
             fs_
     in  ( _lhsOp,_lhsOself))
-- Prods -------------------------------------------------------
type Prods = [Prod]
-- cata
sem_Prods :: Prods ->
             T_Prods
sem_Prods list =
    (Prelude.foldr sem_Prods_Cons sem_Prods_Nil (Prelude.map sem_Prod list))
-- semantic domain
type T_Prods = ( ([String]),Prods)
data Inh_Prods = Inh_Prods {}
data Syn_Prods = Syn_Prods {plns_Syn_Prods :: ([String]),self_Syn_Prods :: Prods}
wrap_Prods :: T_Prods ->
              Inh_Prods ->
              Syn_Prods
wrap_Prods sem (Inh_Prods) =
    (let ( _lhsOplns,_lhsOself) = sem
     in  (Syn_Prods _lhsOplns _lhsOself))
sem_Prods_Cons :: T_Prod ->
                  T_Prods ->
                  T_Prods
sem_Prods_Cons hd_ tl_ =
    (let _lhsOplns :: ([String])
         _lhsOself :: Prods
         _hdIp :: String
         _hdIself :: Prod
         _tlIplns :: ([String])
         _tlIself :: Prods
         _lhsOplns =
             ({-# LINE 60 "./Gen.ag" #-}
              _hdIp : _tlIplns
              {-# LINE 443 "Gen.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIp,_hdIself) =
             hd_
         ( _tlIplns,_tlIself) =
             tl_
     in  ( _lhsOplns,_lhsOself))
sem_Prods_Nil :: T_Prods
sem_Prods_Nil =
    (let _lhsOplns :: ([String])
         _lhsOself :: Prods
         _lhsOplns =
             ({-# LINE 51 "./Gen.ag" #-}
              []
              {-# LINE 461 "Gen.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOplns,_lhsOself))
-- Production --------------------------------------------------
data Production = Production (Node) (String) (Nodes)
                deriving ( Show)
-- cata
sem_Production :: Production ->
                  T_Production
sem_Production (Production _nt _cs _fs) =
    (sem_Production_Production (sem_Node _nt) _cs (sem_Nodes _fs))
-- semantic domain
type T_Production = ( ([String]),Prods,Production)
data Inh_Production = Inh_Production {}
data Syn_Production = Syn_Production {errs_Syn_Production :: ([String]),prods_Syn_Production :: Prods,self_Syn_Production :: Production}
wrap_Production :: T_Production ->
                   Inh_Production ->
                   Syn_Production
wrap_Production sem (Inh_Production) =
    (let ( _lhsOerrs,_lhsOprods,_lhsOself) = sem
     in  (Syn_Production _lhsOerrs _lhsOprods _lhsOself))
sem_Production_Production :: T_Node ->
                             String ->
                             T_Nodes ->
                             T_Production
sem_Production_Production nt_ cs_ fs_ =
    (let _lhsOprods :: Prods
         _lhsOerrs :: ([String])
         _lhsOself :: Production
         _ntIid :: String
         _ntIself :: Node
         _fsIself :: Nodes
         _lhsOprods =
             ({-# LINE 36 "./Gen.ag" #-}
              if null _errs
                  then [Prod (NT _ntIid) cs_ _fsIself]
                  else []
              {-# LINE 502 "Gen.hs" #-}
              )
         _errs =
             ({-# LINE 40 "./Gen.ag" #-}
              case _ntIself of
                  NT _ -> []
                  _    -> error "Terminal on LHS, check Parser"
              {-# LINE 509 "Gen.hs" #-}
              )
         _lhsOerrs =
             ({-# LINE 28 "./Gen.ag" #-}
              _errs
              {-# LINE 514 "Gen.hs" #-}
              )
         _self =
             Production _ntIself cs_ _fsIself
         _lhsOself =
             _self
         ( _ntIid,_ntIself) =
             nt_
         ( _fsIself) =
             fs_
     in  ( _lhsOerrs,_lhsOprods,_lhsOself))
-- Productions -------------------------------------------------
type Productions = [Production]
-- cata
sem_Productions :: Productions ->
                   T_Productions
sem_Productions list =
    (Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list))
-- semantic domain
type T_Productions = ( ([String]),Prods,Productions)
data Inh_Productions = Inh_Productions {}
data Syn_Productions = Syn_Productions {errs_Syn_Productions :: ([String]),prods_Syn_Productions :: Prods,self_Syn_Productions :: Productions}
wrap_Productions :: T_Productions ->
                    Inh_Productions ->
                    Syn_Productions
wrap_Productions sem (Inh_Productions) =
    (let ( _lhsOerrs,_lhsOprods,_lhsOself) = sem
     in  (Syn_Productions _lhsOerrs _lhsOprods _lhsOself))
sem_Productions_Cons :: T_Production ->
                        T_Productions ->
                        T_Productions
sem_Productions_Cons hd_ tl_ =
    (let _lhsOerrs :: ([String])
         _lhsOprods :: Prods
         _lhsOself :: Productions
         _hdIerrs :: ([String])
         _hdIprods :: Prods
         _hdIself :: Production
         _tlIerrs :: ([String])
         _tlIprods :: Prods
         _tlIself :: Productions
         _lhsOerrs =
             ({-# LINE 28 "./Gen.ag" #-}
              ((++) _hdIerrs _tlIerrs)
              {-# LINE 558 "Gen.hs" #-}
              )
         _lhsOprods =
             ({-# LINE 27 "./Gen.ag" #-}
              ((++) _hdIprods _tlIprods)
              {-# LINE 563 "Gen.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIerrs,_hdIprods,_hdIself) =
             hd_
         ( _tlIerrs,_tlIprods,_tlIself) =
             tl_
     in  ( _lhsOerrs,_lhsOprods,_lhsOself))
sem_Productions_Nil :: T_Productions
sem_Productions_Nil =
    (let _lhsOerrs :: ([String])
         _lhsOprods :: Prods
         _lhsOself :: Productions
         _lhsOerrs =
             ({-# LINE 28 "./Gen.ag" #-}
              []
              {-# LINE 582 "Gen.hs" #-}
              )
         _lhsOprods =
             ({-# LINE 27 "./Gen.ag" #-}
              []
              {-# LINE 587 "Gen.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOerrs,_lhsOprods,_lhsOself))
-- Root --------------------------------------------------------
data Root = Root (Grammar)
-- cata
sem_Root :: Root ->
            T_Root
sem_Root (Root _grammar) =
    (sem_Root_Root (sem_Grammar _grammar))
-- semantic domain
type T_Root = ( ([String]),Prods,Root)
data Inh_Root = Inh_Root {}
data Syn_Root = Syn_Root {errs_Syn_Root :: ([String]),prods_Syn_Root :: Prods,self_Syn_Root :: Root}
wrap_Root :: T_Root ->
             Inh_Root ->
             Syn_Root
wrap_Root sem (Inh_Root) =
    (let ( _lhsOerrs,_lhsOprods,_lhsOself) = sem
     in  (Syn_Root _lhsOerrs _lhsOprods _lhsOself))
sem_Root_Root :: T_Grammar ->
                 T_Root
sem_Root_Root grammar_ =
    (let _lhsOerrs :: ([String])
         _lhsOprods :: Prods
         _lhsOself :: Root
         _grammarIerrs :: ([String])
         _grammarIprods :: Prods
         _grammarIself :: Grammar
         _lhsOerrs =
             ({-# LINE 28 "./Gen.ag" #-}
              _grammarIerrs
              {-# LINE 623 "Gen.hs" #-}
              )
         _lhsOprods =
             ({-# LINE 27 "./Gen.ag" #-}
              _grammarIprods
              {-# LINE 628 "Gen.hs" #-}
              )
         _self =
             Root _grammarIself
         _lhsOself =
             _self
         ( _grammarIerrs,_grammarIprods,_grammarIself) =
             grammar_
     in  ( _lhsOerrs,_lhsOprods,_lhsOself))