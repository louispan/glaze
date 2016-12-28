{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Glaze where

import Control.Lens
import Data.Proxy

data Renderer a r = Renderer
    { rendererRenderedMeta :: r
    , rendererValueRenderer :: a -> r
    } deriving Functor
makeFields ''Renderer

instance Applicative (Renderer a) where
    pure a = Renderer a (const a)
    x <*> y = Renderer
        ((x ^. renderedMeta) (y ^. renderedMeta))
        (\a -> (x ^. valueRenderer) a ((y ^. valueRenderer) a))

mkRendererM :: Monad m => (Proxy a -> m (Renderer a r)) -> Getter s a -> m (Renderer s r)
mkRendererM f lns = do
    rdr <- f (Proxy :: Proxy a)
    pure $ Renderer
        (rdr ^. renderedMeta)
        (\s -> (rdr ^. valueRenderer) (s ^. lns))

mkRenderer :: (Proxy a -> Renderer a r) -> Getter s a -> Renderer s r
mkRenderer f lns = Renderer
        (rdr ^. renderedMeta)
        (\s -> (rdr ^. valueRenderer) (s ^. lns))
  where
    rdr = f (Proxy :: Proxy a)

renderWith :: (r -> r -> r) -> Renderer a r -> a -> r
renderWith mainWrapper rdr a =
    mainWrapper (rdr ^. renderedMeta) ((rdr ^. valueRenderer) a)

listRenderer :: ([r] -> r, [r] -> r, [r] -> r)
    -> r
    -> [Renderer a r]
    -> Renderer [a] r
listRenderer (mainWrapper, headerRowWrapper, valueRowWrapper) meta rs =
    Renderer meta listRenderer'
  where
      rdr = sequenceA rs
      listRenderer' as =
          mainWrapper $
          headerRowWrapper (rdr ^. renderedMeta) :
          (valueRowWrapper . view valueRenderer rdr <$> as)

fieldsRenderer :: ([(r, r)] -> r) -> r -> [Renderer a r] -> Renderer a r
fieldsRenderer wrapper meta rs =
    Renderer meta (\a -> wrapper $ (\r -> (r ^. renderedMeta, (r ^. valueRenderer) a)) <$> rs)

rendererM ::
  (Traversable t, Monad m) =>
  (wrapper -> meta -> t rs -> r) -> m wrapper -> m meta -> t (m rs) -> m r
rendererM f wrapper meta rs = do
    wrapper' <- wrapper
    meta' <- meta
    rs' <- sequenceA rs
    pure $ f wrapper' meta' rs'

-- (wrap table around rows, header fields to header row, value fields to value row)
listRendererM ::
  Monad m =>
  m ([r] -> r, [r] -> r, [r] -> r)
  -> m r -> [m (Renderer a r)] -> m (Renderer [a] r)
listRendererM = rendererM listRenderer

-- ([(header, value)] -> render)
fieldsRendererM ::
  Monad m =>
  m ([(r, r)] -> r) -> m r -> [m (Renderer a r)] -> m (Renderer a r)
fieldsRendererM = rendererM fieldsRenderer
