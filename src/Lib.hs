{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Lib
    ( populateCatalog
    , selectNode
    , filterNodes
    , transformNodeList
    , nodeData
    , nodeDetails
    , catalogTpl
    ) where

import qualified Data.Int     as DI
import           Data.IntMap  (IntMap)
import           Data.List    (transpose)
import qualified Data.List    as List
import           Data.Text    (Text)
import           Data.Tree    (Tree (..))
import qualified Data.Tree    as Tree
import           GHC.Generics

newtype CatalogNodeID = CatalogNodeID DI.Int32
  deriving stock (Eq, Show, Generic)

data CatalogNode item = CatalogNode
    { cnLabel :: !Text
    , cnItems :: [ item ]
    }
  deriving stock (Eq, Show, Generic)

newtype CatalogID = CatalogID DI.Int32
  deriving stock (Eq, Show, Generic)

newtype CourseID = CourseID DI.Int32
                   deriving stock (Eq, Show, Generic)

newtype CorporateID = CorporateID DI.Int32
                      deriving stock (Eq, Show, Generic)

newtype CourseShort = CourseShort Text
                      deriving (Eq, Show, Generic)
-- ^ NOTE: we present (Catalog, CatalogID) :: DI.Int32
data Catalog item = Catalog
  { cLabel     :: !Text
  , cRefLocale :: !(Maybe Text)
  , cTaxonomy  :: [CourseTree item]
  }
  deriving stock (Eq, Show, Generic)

--- Data ---

nodeData = [
            [ (CatalogNodeID 5, CatalogNode "test node 2-1" [])
            , (CatalogNodeID 3, CatalogNode "test node 2" [])
            , (CatalogNodeID 2, CatalogNode "test catalog node" [])
            ],
            [ (CatalogNodeID 3, CatalogNode "test node 2" [])
            , (CatalogNodeID 2, CatalogNode "test catalog node" [])
            ],
            [ (CatalogNodeID 4, CatalogNode "test node 3" [])
            , (CatalogNodeID 2, CatalogNode "test catalog node" [])
            ],
            [ (CatalogNodeID 2, CatalogNode "test catalog node" [])
            ]
          ]

nodeDetails = [ (CatalogNodeID 2, (CourseID 1, CourseShort "test 1"))
             , (CatalogNodeID 3, (CourseID 3, CourseShort "test 3"))
             , (CatalogNodeID 4, (CourseID 2, CourseShort "test 2"))
             , (CatalogNodeID 5, (CourseID 4, CourseShort "test 4"))
             , (CatalogNodeID 5, (CourseID 5, CourseShort "test 5"))
             ]

catalogTpl = (CorporateID 1, (CatalogID 2, Catalog "testLabel" (Just "en") []))

--- API ---

transformNodeList :: Eq a => [[a]] -> [[a]]
transformNodeList = map (List.nub . asLast)
   where asLast = undefined
--    --- asTrans xs = map (List.nub . asLast) xs
--    asLast [] = []
--    asLast ys = last ys

type CourseItem = (CourseID, CourseShort)
type CatalogItem = (CatalogID, Catalog CourseItem)
type CatalogMap = IntMap [CatalogItem]
type CourseNode item = (CatalogNodeID, CatalogNode item)

type CourseTree item = Tree (CourseNode item)


populateCatalog :: (CorporateID, CatalogItem)
                -> [[CourseNode CourseItem]]
                -> [(CatalogNodeID, CourseItem)]
                -> (CorporateID, CatalogItem)
populateCatalog corpTpl nodes catNodeList = foldr asCatalogItems corpTpl setNodeList
  where
    setNodeList = filterNodes nodes

    newTree a = Node (absorbVector a) [] -- $ filter (\(cId, _) -> cId == catNid) catNodeList
    absorbVector :: CourseNode CourseItem -> CourseNode CourseItem
    absorbVector (cnId, cTpl) = foldr (asCourseNode cnId) (cnId, cTpl) catNodeList

    asCourseNode cnId  (cnId', courseTpl) (cnId'', cn@CatalogNode {..})
      | cnId == cnId' && cnId == cnId'' = (cnId, cn { cnItems =  courseTpl : cnItems })
      | otherwise = (cnId, cn)

    asCatalogItems :: [CourseNode CourseItem] -> (CorporateID, CatalogItem) -> (CorporateID, CatalogItem)
    asCatalogItems [] cTpl = cTpl
    asCatalogItems (fstNode : catNodes) (corpId, (catId, cat@Catalog {..})) = (corpId, (catId, cat { cTaxonomy = (List.foldl' addToForest (newTree fstNode) catNodes) : cTaxonomy }))
      where
          addToForest :: CourseTree CourseItem
                      -> CourseNode CourseItem
                      -> CourseTree CourseItem
          addToForest cTree cNode = Node (absorbVector cNode) [cTree]

filterNodes ::  [[CourseNode CourseItem]]
             -> [[CourseNode CourseItem]]
filterNodes = List.foldl' selectNode []

-- | 'selectNode' select only longest list without it prefixes
selectNode :: [[CourseNode CourseItem]]
           -> [CourseNode CourseItem]
           -> [[CourseNode CourseItem]]
selectNode acc its =
  case List.find isSubNode acc of
      Nothing -> its:acc
      Just _  -> acc
  where
    isSubNode acIt
      | its `List.isSuffixOf` acIt =  True
      | otherwise = False

