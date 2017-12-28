module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

type AddressBook = List Entry

type Entry =
    { firstName :: String
    , lastName :: String
    , address :: Address
    }

type Address =
    { street :: String
    , city :: String
    , zip :: String
    }

sandro :: Entry
sandro = {
    firstName: "Sandro",
    lastName: "Dolidze",
    address: {
        street: "11 Tamarashvili Street",
        city: "Tbilsi",
        zip: "0179"
    }
}

showEntry :: Entry -> String
showEntry entry =
    entry.lastName <> ", " <>
    entry.firstName <> ": " <>
    showAddress entry.address

showAddress :: Address -> String
showAddress address =
    address.street <> ", " <>
    address.city <> ", " <>
    address.zip

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = 
    head $
    filter (\entry -> entry.firstName == firstName && entry.lastName == lastName) book