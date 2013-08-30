

-- UUAGC 0.9.42.3 (AG/Main.ag)
module AG.Main where

{-# LINE 7 "./AG/Main.ag" #-}

import Data.List (groupBy, intercalate)
import Data.Char (toUpper, toLower)
import CCO.Component (Component, component)
{-# LINE 12 "AG/Main.hs" #-}
{-# LINE 15 "./AG/ParseGen.ag" #-}

consParser :: String -> Nodes -> String
consParser c fs = consName ++ app ++ fieldParser fs
    where   consName = map toUpper c
            app      = case head fs of
                        (T _) -> " <$ "
                        _     -> " <$> " 
fieldParser :: Nodes -> String
fieldParser []      = ""
fieldParser [f]     = showP f 
fieldParser (f:fs)  = showP f ++ conc (head fs) ++ fieldParser fs
    where conc (T x)= " <* "
          conc _    = " <*> "

showP (NT nt)    = "p" ++ map toUpper nt
showP (T tm)     = "pTm \"" ++ tm ++ "\""
showP (BoolLit)  = "pBool"
showP (IntLit)   = "pInt"

{-# LINE 33 "AG/Main.hs" #-}

{-# LINE 22 "./AG/HS.ag" #-}

hasType :: Node -> Bool
hasType (T _) = False
hasType _     = True

typeName :: Node -> String
typeName (NT nt) = map toUpper nt
typeName BoolLit = "Bool"
typeName IntLit  = "Int"
{-# LINE 45 "AG/Main.hs" #-}

{-# LINE 16 "./AG/Validate.ag" #-}

validate :: Grammar -> IO (Maybe Document)
validate grammar = do 
    let wrapper = (wrap_Root (sem_Root $ Root grammar) Inh_Root{})
    let errors  = errs_Syn_Root     wrapper
    let prods   = prods_Syn_Root    wrapper
    let tms     = tms_Syn_Root      wrapper
    let dts     = map (\(p@(Production nt _ _):pds) -> DataType nt (p:pds)) $ 
                    groupBy (\(Production n1 _ _) (Production n2 _ _) -> 
                        n1 == n2) prods
    let root    = (\(DataType (NT r) _) -> r) $ head dts 
    case errors of
        []  -> return $ Just $ Document (docHeader root) dts tms
        _   -> putStrLn (unlines errors) >> return Nothing
{-# LINE 62 "AG/Main.hs" #-}

{-# LINE 41 "./AG/Main.ag" #-}

generate :: String -> Document -> IO ()
generate path (Document head dts tms) = do 
    lexer <- readFile "utils/Lexer.hs"
    writeFile (path ++ "Lexer.hs") lexer
    writeFile (path ++ "Data.hs") terminals
    writeFile (path ++ "Sem.ag") ag
    writeFile (path ++ "Parser.hs") parser
    writeFile (path ++ "Makefile") mk
    writeFile (path ++ "Main.hs") mf
      where
        mf        = unlines $
                        [ "module Main where"
                        , "import CCO.Component (ioWrap, component)"
                        , "import Parser (parser)"
                        , "import Sem"
                        , "import Control.Arrow ((>>>))"
                        , "main = ioWrap $ parser >>> component (\\tree -> return $ show tree)"
                        ]
        mk        = unlines $ 
                        [ "all :"
                        , "\tuuagc -Hdcfws --self Sem.ag"
                        , "\tghc --make Main.hs -o parser"
                        , "\trm *.hi *.o"
                        ]
        ag        = unlines $
                        [ "module{Sem} {} {}"       
                        ] ++ dtypes ++ sems ++ drvs
        nts       = [ nt | (DataType (NT nt) _) <- dts]
        drvs      = map (\t -> "deriving " ++ map toUpper t ++ " : Show") nts
        sems      = []
        dtypes    = hs_Syn_DataTypes wrapper
        terminals = 
            unlines  [ "module Data where"
                     , "terminals = [\"" ++ (intercalate "\",\"" tms) ++ "\"]"
                     ]
        parser = head ++ "\n" ++ unlines pLines
        pLines = plns_Syn_DataTypes wrapper
        wrapper= wrap_DataTypes (sem_DataTypes dts) Inh_DataTypes{}

docHeader r =  unlines  [ "module Parser where"
                        , "import Sem"
                        , "import Lexer"
                        , "import CCO.Component as C"
                        , "import CCO.Parsing (Parser(..), eof)"
                        , "import Control.Applicative"
                        , "parser = C.parser lexer (p" ++ (map toUpper r) ++ " <* eof)"
                        ]

{-# LINE 114 "AG/Main.hs" #-}
-- DataType ----------------------------------------------------
data DataType = DataType (Node) (Productions)
-- cata
sem_DataType :: DataType ->
                T_DataType
sem_DataType (DataType _nt _ps) =
    (sem_DataType_DataType (sem_Node _nt) (sem_Productions _ps))
-- semantic domain
type T_DataType = ( ([String]),String,DataType)
data Inh_DataType = Inh_DataType {}
data Syn_DataType = Syn_DataType {hs_Syn_DataType :: ([String]),p_Syn_DataType :: String,self_Syn_DataType :: DataType}
wrap_DataType :: T_DataType ->
                 Inh_DataType ->
                 Syn_DataType
wrap_DataType sem (Inh_DataType) =
    (let ( _lhsOhs,_lhsOp,_lhsOself) = sem
     in  (Syn_DataType _lhsOhs _lhsOp _lhsOself))
sem_DataType_DataType :: T_Node ->
                         T_Productions ->
                         T_DataType
sem_DataType_DataType nt_ ps_ =
    (let _lhsOhs :: ([String])
         _lhsOself :: DataType
         _lhsOp :: String
         _ntIid :: String
         _ntIself :: Node
         _ntItms :: ([String])
         _psIerrs :: ([String])
         _psIhs :: ([String])
         _psIplns :: ([String])
         _psIself :: Productions
         _psItms :: ([String])
         _p =
             ({-# LINE 9 "./AG/ParseGen.ag" #-}
              showP _ntIself ++ " = " ++ _pCs
              {-# LINE 150 "AG/Main.hs" #-}
              )
         _pCs =
             ({-# LINE 10 "./AG/ParseGen.ag" #-}
              intercalate " <|> " _psIplns
              {-# LINE 155 "AG/Main.hs" #-}
              )
         _lhsOhs =
             ({-# LINE 15 "./AG/HS.ag" #-}
              ["data " ++ (map toUpper _ntIid) ++ unlines _psIhs]
              {-# LINE 160 "AG/Main.hs" #-}
              )
         _self =
             DataType _ntIself _psIself
         _lhsOself =
             _self
         _lhsOp =
             ({-# LINE 5 "./AG/ParseGen.ag" #-}
              _p
              {-# LINE 169 "AG/Main.hs" #-}
              )
         ( _ntIid,_ntIself,_ntItms) =
             nt_
         ( _psIerrs,_psIhs,_psIplns,_psIself,_psItms) =
             ps_
     in  ( _lhsOhs,_lhsOp,_lhsOself))
-- DataTypes ---------------------------------------------------
type DataTypes = [DataType]
-- cata
sem_DataTypes :: DataTypes ->
                 T_DataTypes
sem_DataTypes list =
    (Prelude.foldr sem_DataTypes_Cons sem_DataTypes_Nil (Prelude.map sem_DataType list))
-- semantic domain
type T_DataTypes = ( ([String]),([String]),DataTypes)
data Inh_DataTypes = Inh_DataTypes {}
data Syn_DataTypes = Syn_DataTypes {hs_Syn_DataTypes :: ([String]),plns_Syn_DataTypes :: ([String]),self_Syn_DataTypes :: DataTypes}
wrap_DataTypes :: T_DataTypes ->
                  Inh_DataTypes ->
                  Syn_DataTypes
wrap_DataTypes sem (Inh_DataTypes) =
    (let ( _lhsOhs,_lhsOplns,_lhsOself) = sem
     in  (Syn_DataTypes _lhsOhs _lhsOplns _lhsOself))
sem_DataTypes_Cons :: T_DataType ->
                      T_DataTypes ->
                      T_DataTypes
sem_DataTypes_Cons hd_ tl_ =
    (let _lhsOplns :: ([String])
         _lhsOhs :: ([String])
         _lhsOself :: DataTypes
         _hdIhs :: ([String])
         _hdIp :: String
         _hdIself :: DataType
         _tlIhs :: ([String])
         _tlIplns :: ([String])
         _tlIself :: DataTypes
         _lhsOplns =
             ({-# LINE 7 "./AG/ParseGen.ag" #-}
              _hdIp : _tlIplns
              {-# LINE 209 "AG/Main.hs" #-}
              )
         _lhsOhs =
             ({-# LINE 13 "./AG/HS.ag" #-}
              ((++) _hdIhs _tlIhs)
              {-# LINE 214 "AG/Main.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIhs,_hdIp,_hdIself) =
             hd_
         ( _tlIhs,_tlIplns,_tlIself) =
             tl_
     in  ( _lhsOhs,_lhsOplns,_lhsOself))
sem_DataTypes_Nil :: T_DataTypes
sem_DataTypes_Nil =
    (let _lhsOhs :: ([String])
         _lhsOplns :: ([String])
         _lhsOself :: DataTypes
         _lhsOhs =
             ({-# LINE 13 "./AG/HS.ag" #-}
              []
              {-# LINE 233 "AG/Main.hs" #-}
              )
         _lhsOplns =
             ({-# LINE 3 "./AG/ParseGen.ag" #-}
              []
              {-# LINE 238 "AG/Main.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOhs,_lhsOplns,_lhsOself))
-- Document ----------------------------------------------------
data Document = Document (String) (DataTypes) (([String]))
-- cata
sem_Document :: Document ->
                T_Document
sem_Document (Document _head _dts _tms) =
    (sem_Document_Document _head (sem_DataTypes _dts) _tms)
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
                         T_DataTypes ->
                         ([String]) ->
                         T_Document
sem_Document_Document head_ dts_ tms_ =
    (let _lhsOself :: Document
         _dtsIhs :: ([String])
         _dtsIplns :: ([String])
         _dtsIself :: DataTypes
         _self =
             Document head_ _dtsIself tms_
         _lhsOself =
             _self
         ( _dtsIhs,_dtsIplns,_dtsIself) =
             dts_
     in  ( _lhsOself))
-- Grammar -----------------------------------------------------
data Grammar = Grammar (Productions)
             deriving ( Show)
-- cata
sem_Grammar :: Grammar ->
               T_Grammar
sem_Grammar (Grammar _ps) =
    (sem_Grammar_Grammar (sem_Productions _ps))
-- semantic domain
type T_Grammar = ( ([String]),Productions,Grammar,([String]))
data Inh_Grammar = Inh_Grammar {}
data Syn_Grammar = Syn_Grammar {errs_Syn_Grammar :: ([String]),prods_Syn_Grammar :: Productions,self_Syn_Grammar :: Grammar,tms_Syn_Grammar :: ([String])}
wrap_Grammar :: T_Grammar ->
                Inh_Grammar ->
                Syn_Grammar
wrap_Grammar sem (Inh_Grammar) =
    (let ( _lhsOerrs,_lhsOprods,_lhsOself,_lhsOtms) = sem
     in  (Syn_Grammar _lhsOerrs _lhsOprods _lhsOself _lhsOtms))
sem_Grammar_Grammar :: T_Productions ->
                       T_Grammar
sem_Grammar_Grammar ps_ =
    (let _lhsOprods :: Productions
         _lhsOerrs :: ([String])
         _lhsOtms :: ([String])
         _lhsOself :: Grammar
         _psIerrs :: ([String])
         _psIhs :: ([String])
         _psIplns :: ([String])
         _psIself :: Productions
         _psItms :: ([String])
         _lhsOprods =
             ({-# LINE 13 "./AG/Validate.ag" #-}
              _psIself
              {-# LINE 311 "AG/Main.hs" #-}
              )
         _lhsOerrs =
             ({-# LINE 7 "./AG/Validate.ag" #-}
              _psIerrs
              {-# LINE 316 "AG/Main.hs" #-}
              )
         _lhsOtms =
             ({-# LINE 9 "./AG/Validate.ag" #-}
              _psItms
              {-# LINE 321 "AG/Main.hs" #-}
              )
         _self =
             Grammar _psIself
         _lhsOself =
             _self
         ( _psIerrs,_psIhs,_psIplns,_psIself,_psItms) =
             ps_
     in  ( _lhsOerrs,_lhsOprods,_lhsOself,_lhsOtms))
-- Node --------------------------------------------------------
data Node = NT (String)
          | T (String)
          | BoolLit
          | IntLit
          deriving ( Eq,Show)
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
type T_Node = ( String,Node,([String]))
data Inh_Node = Inh_Node {}
data Syn_Node = Syn_Node {id_Syn_Node :: String,self_Syn_Node :: Node,tms_Syn_Node :: ([String])}
wrap_Node :: T_Node ->
             Inh_Node ->
             Syn_Node
wrap_Node sem (Inh_Node) =
    (let ( _lhsOid,_lhsOself,_lhsOtms) = sem
     in  (Syn_Node _lhsOid _lhsOself _lhsOtms))
sem_Node_NT :: String ->
               T_Node
sem_Node_NT ident_ =
    (let _lhsOid :: String
         _lhsOtms :: ([String])
         _lhsOself :: Node
         _lhsOid =
             ({-# LINE 36 "./AG/Main.ag" #-}
              ident_
              {-# LINE 366 "AG/Main.hs" #-}
              )
         _lhsOtms =
             ({-# LINE 11 "./AG/Validate.ag" #-}
              []
              {-# LINE 371 "AG/Main.hs" #-}
              )
         _self =
             NT ident_
         _lhsOself =
             _self
     in  ( _lhsOid,_lhsOself,_lhsOtms))
sem_Node_T :: String ->
              T_Node
sem_Node_T ident_ =
    (let _lhsOtms :: ([String])
         _lhsOid :: String
         _lhsOself :: Node
         _lhsOtms =
             ({-# LINE 14 "./AG/Validate.ag" #-}
              [ident_]
              {-# LINE 387 "AG/Main.hs" #-}
              )
         _lhsOid =
             ({-# LINE 37 "./AG/Main.ag" #-}
              ident_
              {-# LINE 392 "AG/Main.hs" #-}
              )
         _self =
             T ident_
         _lhsOself =
             _self
     in  ( _lhsOid,_lhsOself,_lhsOtms))
sem_Node_BoolLit :: T_Node
sem_Node_BoolLit =
    (let _lhsOid :: String
         _lhsOtms :: ([String])
         _lhsOself :: Node
         _lhsOid =
             ({-# LINE 38 "./AG/Main.ag" #-}
              "Bool"
              {-# LINE 407 "AG/Main.hs" #-}
              )
         _lhsOtms =
             ({-# LINE 11 "./AG/Validate.ag" #-}
              []
              {-# LINE 412 "AG/Main.hs" #-}
              )
         _self =
             BoolLit
         _lhsOself =
             _self
     in  ( _lhsOid,_lhsOself,_lhsOtms))
sem_Node_IntLit :: T_Node
sem_Node_IntLit =
    (let _lhsOid :: String
         _lhsOtms :: ([String])
         _lhsOself :: Node
         _lhsOid =
             ({-# LINE 39 "./AG/Main.ag" #-}
              "Int"
              {-# LINE 427 "AG/Main.hs" #-}
              )
         _lhsOtms =
             ({-# LINE 11 "./AG/Validate.ag" #-}
              []
              {-# LINE 432 "AG/Main.hs" #-}
              )
         _self =
             IntLit
         _lhsOself =
             _self
     in  ( _lhsOid,_lhsOself,_lhsOtms))
-- Nodes -------------------------------------------------------
type Nodes = [Node]
-- cata
sem_Nodes :: Nodes ->
             T_Nodes
sem_Nodes list =
    (Prelude.foldr sem_Nodes_Cons sem_Nodes_Nil (Prelude.map sem_Node list))
-- semantic domain
type T_Nodes = ( ([String]),Nodes,([String]))
data Inh_Nodes = Inh_Nodes {}
data Syn_Nodes = Syn_Nodes {fs_Syn_Nodes :: ([String]),self_Syn_Nodes :: Nodes,tms_Syn_Nodes :: ([String])}
wrap_Nodes :: T_Nodes ->
              Inh_Nodes ->
              Syn_Nodes
wrap_Nodes sem (Inh_Nodes) =
    (let ( _lhsOfs,_lhsOself,_lhsOtms) = sem
     in  (Syn_Nodes _lhsOfs _lhsOself _lhsOtms))
sem_Nodes_Cons :: T_Node ->
                  T_Nodes ->
                  T_Nodes
sem_Nodes_Cons hd_ tl_ =
    (let _lhsOfs :: ([String])
         _lhsOtms :: ([String])
         _lhsOself :: Nodes
         _hdIid :: String
         _hdIself :: Node
         _hdItms :: ([String])
         _tlIfs :: ([String])
         _tlIself :: Nodes
         _tlItms :: ([String])
         _lhsOfs =
             ({-# LINE 6 "./AG/HS.ag" #-}
              (if hasType _hdIself then [typeName _hdIself] else []) ++ _tlIfs
              {-# LINE 472 "AG/Main.hs" #-}
              )
         _lhsOtms =
             ({-# LINE 11 "./AG/Validate.ag" #-}
              ((++) _hdItms _tlItms)
              {-# LINE 477 "AG/Main.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIid,_hdIself,_hdItms) =
             hd_
         ( _tlIfs,_tlIself,_tlItms) =
             tl_
     in  ( _lhsOfs,_lhsOself,_lhsOtms))
sem_Nodes_Nil :: T_Nodes
sem_Nodes_Nil =
    (let _lhsOfs :: ([String])
         _lhsOtms :: ([String])
         _lhsOself :: Nodes
         _lhsOfs =
             ({-# LINE 8 "./AG/HS.ag" #-}
              []
              {-# LINE 496 "AG/Main.hs" #-}
              )
         _lhsOtms =
             ({-# LINE 11 "./AG/Validate.ag" #-}
              []
              {-# LINE 501 "AG/Main.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOfs,_lhsOself,_lhsOtms))
-- Production --------------------------------------------------
data Production = Production (Node) (String) (Nodes)
                deriving ( Show)
-- cata
sem_Production :: Production ->
                  T_Production
sem_Production (Production _nt _cs _fs) =
    (sem_Production_Production (sem_Node _nt) _cs (sem_Nodes _fs))
-- semantic domain
type T_Production = ( ([String]),([String]),String,Production,([String]))
data Inh_Production = Inh_Production {}
data Syn_Production = Syn_Production {errs_Syn_Production :: ([String]),hs_Syn_Production :: ([String]),p_Syn_Production :: String,self_Syn_Production :: Production,tms_Syn_Production :: ([String])}
wrap_Production :: T_Production ->
                   Inh_Production ->
                   Syn_Production
wrap_Production sem (Inh_Production) =
    (let ( _lhsOerrs,_lhsOhs,_lhsOp,_lhsOself,_lhsOtms) = sem
     in  (Syn_Production _lhsOerrs _lhsOhs _lhsOp _lhsOself _lhsOtms))
sem_Production_Production :: T_Node ->
                             String ->
                             T_Nodes ->
                             T_Production
sem_Production_Production nt_ cs_ fs_ =
    (let _lhsOp :: String
         _lhsOhs :: ([String])
         _lhsOerrs :: ([String])
         _lhsOtms :: ([String])
         _lhsOself :: Production
         _ntIid :: String
         _ntIself :: Node
         _ntItms :: ([String])
         _fsIfs :: ([String])
         _fsIself :: Nodes
         _fsItms :: ([String])
         _lhsOp =
             ({-# LINE 13 "./AG/ParseGen.ag" #-}
              "(" ++ consParser cs_ _fsIself ++ ")"
              {-# LINE 545 "AG/Main.hs" #-}
              )
         _lhsOhs =
             ({-# LINE 18 "./AG/HS.ag" #-}
              ["| " ++ map toUpper cs_ ++ "  " ++
                  (intercalate " " (map (\f -> map toLower f ++ " :: " ++ f) _fsIfs))]
              {-# LINE 551 "AG/Main.hs" #-}
              )
         _lhsOerrs =
             ({-# LINE 7 "./AG/Validate.ag" #-}
              []
              {-# LINE 556 "AG/Main.hs" #-}
              )
         _lhsOtms =
             ({-# LINE 9 "./AG/Validate.ag" #-}
              ((++) _ntItms _fsItms)
              {-# LINE 561 "AG/Main.hs" #-}
              )
         _self =
             Production _ntIself cs_ _fsIself
         _lhsOself =
             _self
         ( _ntIid,_ntIself,_ntItms) =
             nt_
         ( _fsIfs,_fsIself,_fsItms) =
             fs_
     in  ( _lhsOerrs,_lhsOhs,_lhsOp,_lhsOself,_lhsOtms))
-- Productions -------------------------------------------------
type Productions = [Production]
-- cata
sem_Productions :: Productions ->
                   T_Productions
sem_Productions list =
    (Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list))
-- semantic domain
type T_Productions = ( ([String]),([String]),([String]),Productions,([String]))
data Inh_Productions = Inh_Productions {}
data Syn_Productions = Syn_Productions {errs_Syn_Productions :: ([String]),hs_Syn_Productions :: ([String]),plns_Syn_Productions :: ([String]),self_Syn_Productions :: Productions,tms_Syn_Productions :: ([String])}
wrap_Productions :: T_Productions ->
                    Inh_Productions ->
                    Syn_Productions
wrap_Productions sem (Inh_Productions) =
    (let ( _lhsOerrs,_lhsOhs,_lhsOplns,_lhsOself,_lhsOtms) = sem
     in  (Syn_Productions _lhsOerrs _lhsOhs _lhsOplns _lhsOself _lhsOtms))
sem_Productions_Cons :: T_Production ->
                        T_Productions ->
                        T_Productions
sem_Productions_Cons hd_ tl_ =
    (let _lhsOplns :: ([String])
         _lhsOerrs :: ([String])
         _lhsOhs :: ([String])
         _lhsOtms :: ([String])
         _lhsOself :: Productions
         _hdIerrs :: ([String])
         _hdIhs :: ([String])
         _hdIp :: String
         _hdIself :: Production
         _hdItms :: ([String])
         _tlIerrs :: ([String])
         _tlIhs :: ([String])
         _tlIplns :: ([String])
         _tlIself :: Productions
         _tlItms :: ([String])
         _lhsOplns =
             ({-# LINE 12 "./AG/ParseGen.ag" #-}
              _hdIp : _tlIplns
              {-# LINE 611 "AG/Main.hs" #-}
              )
         _lhsOerrs =
             ({-# LINE 7 "./AG/Validate.ag" #-}
              ((++) _hdIerrs _tlIerrs)
              {-# LINE 616 "AG/Main.hs" #-}
              )
         _lhsOhs =
             ({-# LINE 13 "./AG/HS.ag" #-}
              ((++) _hdIhs _tlIhs)
              {-# LINE 621 "AG/Main.hs" #-}
              )
         _lhsOtms =
             ({-# LINE 9 "./AG/Validate.ag" #-}
              ((++) _hdItms _tlItms)
              {-# LINE 626 "AG/Main.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIerrs,_hdIhs,_hdIp,_hdIself,_hdItms) =
             hd_
         ( _tlIerrs,_tlIhs,_tlIplns,_tlIself,_tlItms) =
             tl_
     in  ( _lhsOerrs,_lhsOhs,_lhsOplns,_lhsOself,_lhsOtms))
sem_Productions_Nil :: T_Productions
sem_Productions_Nil =
    (let _lhsOerrs :: ([String])
         _lhsOhs :: ([String])
         _lhsOplns :: ([String])
         _lhsOtms :: ([String])
         _lhsOself :: Productions
         _lhsOerrs =
             ({-# LINE 7 "./AG/Validate.ag" #-}
              []
              {-# LINE 647 "AG/Main.hs" #-}
              )
         _lhsOhs =
             ({-# LINE 13 "./AG/HS.ag" #-}
              []
              {-# LINE 652 "AG/Main.hs" #-}
              )
         _lhsOplns =
             ({-# LINE 3 "./AG/ParseGen.ag" #-}
              []
              {-# LINE 657 "AG/Main.hs" #-}
              )
         _lhsOtms =
             ({-# LINE 9 "./AG/Validate.ag" #-}
              []
              {-# LINE 662 "AG/Main.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOerrs,_lhsOhs,_lhsOplns,_lhsOself,_lhsOtms))
-- Root --------------------------------------------------------
data Root = Root (Grammar)
-- cata
sem_Root :: Root ->
            T_Root
sem_Root (Root _grammar) =
    (sem_Root_Root (sem_Grammar _grammar))
-- semantic domain
type T_Root = ( ([String]),Productions,Root,([String]))
data Inh_Root = Inh_Root {}
data Syn_Root = Syn_Root {errs_Syn_Root :: ([String]),prods_Syn_Root :: Productions,self_Syn_Root :: Root,tms_Syn_Root :: ([String])}
wrap_Root :: T_Root ->
             Inh_Root ->
             Syn_Root
wrap_Root sem (Inh_Root) =
    (let ( _lhsOerrs,_lhsOprods,_lhsOself,_lhsOtms) = sem
     in  (Syn_Root _lhsOerrs _lhsOprods _lhsOself _lhsOtms))
sem_Root_Root :: T_Grammar ->
                 T_Root
sem_Root_Root grammar_ =
    (let _lhsOerrs :: ([String])
         _lhsOtms :: ([String])
         _lhsOself :: Root
         _lhsOprods :: Productions
         _grammarIerrs :: ([String])
         _grammarIprods :: Productions
         _grammarIself :: Grammar
         _grammarItms :: ([String])
         _lhsOerrs =
             ({-# LINE 7 "./AG/Validate.ag" #-}
              _grammarIerrs
              {-# LINE 700 "AG/Main.hs" #-}
              )
         _lhsOtms =
             ({-# LINE 9 "./AG/Validate.ag" #-}
              _grammarItms
              {-# LINE 705 "AG/Main.hs" #-}
              )
         _self =
             Root _grammarIself
         _lhsOself =
             _self
         _lhsOprods =
             ({-# LINE 4 "./AG/Validate.ag" #-}
              _grammarIprods
              {-# LINE 714 "AG/Main.hs" #-}
              )
         ( _grammarIerrs,_grammarIprods,_grammarIself,_grammarItms) =
             grammar_
     in  ( _lhsOerrs,_lhsOprods,_lhsOself,_lhsOtms))