module Server
  ( tests
  ) where

import Protolude

import Servant.QuickCheck
       ((<%>), createContainsValidLocation, defaultArgs, not500,
        notLongerThan, serverSatisfies,
        unauthorizedContainsWWWAuthenticate, withServantServer)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (it, testSpec)

import CompareRevisions.API (api, server)


tests :: IO TestTree
tests =
  testSpec "CompareRevisions.Server" $
    it "follows best practices" $
      withServantServer api (pure server) $
      \burl ->
        serverSatisfies
        api
        burl
        defaultArgs
        (not500 <%> createContainsValidLocation <%> notLongerThan 100000000 <%> unauthorizedContainsWWWAuthenticate <%>
         mempty)
