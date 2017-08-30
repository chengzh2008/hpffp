newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

-- but we should not use WriteT for two reasons:
-- 1. WriterT leads memory leaks.
-- 2. Reader lets us talk about values we need, Writer lets us deal with values we can emit and combine, and State lets us both read and write values in any manner we desire.

-- there is also a type in the transformers library that combines Reader, Writer, and Stte into one big type:
newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }


-- StateT and Parser
type Parser a = String -> Maybe (a, String)

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-- alias Parser in terms of StateT
type Parser' = StateT String Maybe

-- ListT is not practically to use. Some libraries such as pipes and conduit do it better for most use-cases.
