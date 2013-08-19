

-- UUAGC 0.9.42.3 (Semantiek.ag)
module Semantiek where

{-# LINE 3 "./Semantiek.ag" #-}

import Prelude ((+), Int, return, max)
import CCO.Component
{-# LINE 11 "Semantiek.hs" #-}
{-# LINE 47 "./Semantiek.ag" #-}

som :: Component Root Int
som = component (\root -> 
        return (sum_Syn_Root (wrap_Root (sem_Root root) Inh_Root))
        )

height :: Component Root Int
height = component (\root -> 
            return (height_Syn_Root (wrap_Root (sem_Root root) Inh_Root))
            )

{-# LINE 24 "Semantiek.hs" #-}
-- Children ----------------------------------------------------
data Children = Just (Node)
              | Some (Node) (Children)
-- cata
sem_Children :: Children ->
                T_Children
sem_Children (Just _node) =
    (sem_Children_Just (sem_Node _node))
sem_Children (Some _node _children) =
    (sem_Children_Some (sem_Node _node) (sem_Children _children))
-- semantic domain
type T_Children = Int ->
                  ( Int,Children,Int)
data Inh_Children = Inh_Children {depth_Inh_Children :: Int}
data Syn_Children = Syn_Children {height_Syn_Children :: Int,self_Syn_Children :: Children,sum_Syn_Children :: Int}
wrap_Children :: T_Children ->
                 Inh_Children ->
                 Syn_Children
wrap_Children sem (Inh_Children _lhsIdepth) =
    (let ( _lhsOheight,_lhsOself,_lhsOsum) = sem _lhsIdepth
     in  (Syn_Children _lhsOheight _lhsOself _lhsOsum))
sem_Children_Just :: T_Node ->
                     T_Children
sem_Children_Just node_ =
    (\ _lhsIdepth ->
         (let _lhsOheight :: Int
              _lhsOself :: Children
              _lhsOsum :: Int
              _nodeOdepth :: Int
              _nodeIheight :: Int
              _nodeIself :: Node
              _nodeIsum :: Int
              _lhsOheight =
                  ({-# LINE 38 "./Semantiek.ag" #-}
                   _nodeIheight
                   {-# LINE 60 "Semantiek.hs" #-}
                   )
              _self =
                  Just _nodeIself
              _lhsOself =
                  _self
              _lhsOsum =
                  ({-# LINE 24 "./Semantiek.ag" #-}
                   _nodeIsum
                   {-# LINE 69 "Semantiek.hs" #-}
                   )
              _nodeOdepth =
                  ({-# LINE 35 "./Semantiek.ag" #-}
                   _lhsIdepth
                   {-# LINE 74 "Semantiek.hs" #-}
                   )
              ( _nodeIheight,_nodeIself,_nodeIsum) =
                  node_ _nodeOdepth
          in  ( _lhsOheight,_lhsOself,_lhsOsum)))
sem_Children_Some :: T_Node ->
                     T_Children ->
                     T_Children
sem_Children_Some node_ children_ =
    (\ _lhsIdepth ->
         (let _lhsOsum :: Int
              _lhsOheight :: Int
              _lhsOself :: Children
              _nodeOdepth :: Int
              _childrenOdepth :: Int
              _nodeIheight :: Int
              _nodeIself :: Node
              _nodeIsum :: Int
              _childrenIheight :: Int
              _childrenIself :: Children
              _childrenIsum :: Int
              _lhsOsum =
                  ({-# LINE 27 "./Semantiek.ag" #-}
                   _nodeIsum + _childrenIsum
                   {-# LINE 98 "Semantiek.hs" #-}
                   )
              _lhsOheight =
                  ({-# LINE 38 "./Semantiek.ag" #-}
                   ((max) _nodeIheight _childrenIheight)
                   {-# LINE 103 "Semantiek.hs" #-}
                   )
              _self =
                  Some _nodeIself _childrenIself
              _lhsOself =
                  _self
              _nodeOdepth =
                  ({-# LINE 35 "./Semantiek.ag" #-}
                   _lhsIdepth
                   {-# LINE 112 "Semantiek.hs" #-}
                   )
              _childrenOdepth =
                  ({-# LINE 35 "./Semantiek.ag" #-}
                   _lhsIdepth
                   {-# LINE 117 "Semantiek.hs" #-}
                   )
              ( _nodeIheight,_nodeIself,_nodeIsum) =
                  node_ _nodeOdepth
              ( _childrenIheight,_childrenIself,_childrenIsum) =
                  children_ _childrenOdepth
          in  ( _lhsOheight,_lhsOself,_lhsOsum)))
-- Node --------------------------------------------------------
data Node = Leaf (Int)
          | Node (Children)
-- cata
sem_Node :: Node ->
            T_Node
sem_Node (Leaf _int) =
    (sem_Node_Leaf _int)
sem_Node (Node _children) =
    (sem_Node_Node (sem_Children _children))
-- semantic domain
type T_Node = Int ->
              ( Int,Node,Int)
data Inh_Node = Inh_Node {depth_Inh_Node :: Int}
data Syn_Node = Syn_Node {height_Syn_Node :: Int,self_Syn_Node :: Node,sum_Syn_Node :: Int}
wrap_Node :: T_Node ->
             Inh_Node ->
             Syn_Node
wrap_Node sem (Inh_Node _lhsIdepth) =
    (let ( _lhsOheight,_lhsOself,_lhsOsum) = sem _lhsIdepth
     in  (Syn_Node _lhsOheight _lhsOself _lhsOsum))
sem_Node_Leaf :: Int ->
                 T_Node
sem_Node_Leaf int_ =
    (\ _lhsIdepth ->
         (let _lhsOsum :: Int
              _lhsOheight :: Int
              _lhsOself :: Node
              _lhsOsum =
                  ({-# LINE 26 "./Semantiek.ag" #-}
                   int_
                   {-# LINE 155 "Semantiek.hs" #-}
                   )
              _lhsOheight =
                  ({-# LINE 38 "./Semantiek.ag" #-}
                   0
                   {-# LINE 160 "Semantiek.hs" #-}
                   )
              _self =
                  Leaf int_
              _lhsOself =
                  _self
          in  ( _lhsOheight,_lhsOself,_lhsOsum)))
sem_Node_Node :: T_Children ->
                 T_Node
sem_Node_Node children_ =
    (\ _lhsIdepth ->
         (let _childrenOdepth :: Int
              _lhsOheight :: Int
              _lhsOself :: Node
              _lhsOsum :: Int
              _childrenIheight :: Int
              _childrenIself :: Children
              _childrenIsum :: Int
              _childrenOdepth =
                  ({-# LINE 41 "./Semantiek.ag" #-}
                   _lhsIdepth + 1
                   {-# LINE 181 "Semantiek.hs" #-}
                   )
              _lhsOheight =
                  ({-# LINE 42 "./Semantiek.ag" #-}
                   _lhsIdepth
                   {-# LINE 186 "Semantiek.hs" #-}
                   )
              _self =
                  Node _childrenIself
              _lhsOself =
                  _self
              _lhsOsum =
                  ({-# LINE 24 "./Semantiek.ag" #-}
                   _childrenIsum
                   {-# LINE 195 "Semantiek.hs" #-}
                   )
              ( _childrenIheight,_childrenIself,_childrenIsum) =
                  children_ _childrenOdepth
          in  ( _lhsOheight,_lhsOself,_lhsOsum)))
-- Root --------------------------------------------------------
data Root = Root (Node)
-- cata
sem_Root :: Root ->
            T_Root
sem_Root (Root _node) =
    (sem_Root_Root (sem_Node _node))
-- semantic domain
type T_Root = ( Int,Root,Int)
data Inh_Root = Inh_Root {}
data Syn_Root = Syn_Root {height_Syn_Root :: Int,self_Syn_Root :: Root,sum_Syn_Root :: Int}
wrap_Root :: T_Root ->
             Inh_Root ->
             Syn_Root
wrap_Root sem (Inh_Root) =
    (let ( _lhsOheight,_lhsOself,_lhsOsum) = sem
     in  (Syn_Root _lhsOheight _lhsOself _lhsOsum))
sem_Root_Root :: T_Node ->
                 T_Root
sem_Root_Root node_ =
    (let _nodeOdepth :: Int
         _lhsOheight :: Int
         _lhsOself :: Root
         _lhsOsum :: Int
         _nodeIheight :: Int
         _nodeIself :: Node
         _nodeIsum :: Int
         _nodeOdepth =
             ({-# LINE 40 "./Semantiek.ag" #-}
              0
              {-# LINE 230 "Semantiek.hs" #-}
              )
         _lhsOheight =
             ({-# LINE 38 "./Semantiek.ag" #-}
              _nodeIheight
              {-# LINE 235 "Semantiek.hs" #-}
              )
         _self =
             Root _nodeIself
         _lhsOself =
             _self
         _lhsOsum =
             ({-# LINE 24 "./Semantiek.ag" #-}
              _nodeIsum
              {-# LINE 244 "Semantiek.hs" #-}
              )
         ( _nodeIheight,_nodeIself,_nodeIsum) =
             node_ _nodeOdepth
     in  ( _lhsOheight,_lhsOself,_lhsOsum))