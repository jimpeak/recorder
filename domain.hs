{-# DeriveGeneric #-}

module Test (
Address(..)
,Agent(..)
,Alternateurls(..)
,Attachmentstyle(..)
,Building(..)
,Buildingparkings(..)
,Buildingtype(..)
,City(..)
,Coolingtype(..)
,Country(..)
,Documenttype(..)
,Exteriorfinish(..)
,Flooringtype(..)
,Foundationtype(..)
,Heatingfuel(..)
,Heatingtype(..)
,Individualdesignation(..)
,Land(..)
,Listingtype(..)
,Log(..)
,Metadata(..)
,Office(..)
,Organization(..)
,Organizationtype(..)
,Parking(..)
,Parkingtype(..)
,Person(..)
,Pooltype(..)
,Property(..)
,Propertyagents(..)
,Propertydetails(..)
,Propertyphoto(..)
,Propertytype(..)
,Provincestate(..)
,Region(..)
,Representationtype(..)
,Roofmaterial(..)
,Room(..)
,Roomtype(..)
,Searchproperty(..)
,Sewertype(..)
,Source(..)
,Subdivision(..)
,Viewtype(..)
,Zoningtype(..)
) where

data Address = Address {
    _id :: !Int
  , _streetaddress :: !Maybe !Text
  , _apartmentnumber :: !Maybe !Text
  , _subdivisionid :: !Maybe !Int
  , _cityid :: !Maybe !Int
  , _provincestatecode :: !Maybe !Text
  , _countryid :: !Maybe !Int
  , _postalcode :: !Maybe !Text
  , _latitude :: !Maybe !Decimal
  , _longitude :: !Maybe !Decimal
} deriving (Generic, Show)

instance ToJSON Address
instance ToJSON Address

instance Recorder Address where
    type Key Address = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM address WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM address"

data Agent = Agent {
    _id :: !Int
  , _firstname :: !Maybe !Text
  , _lastname :: !Maybe !Text
  , _email :: !Maybe !Text
  , _phone1 :: !Maybe !Text
  , _phone2 :: !Maybe !Text
  , _fax :: !Maybe !Text
  , _website :: !Maybe !Text
  , _licensenumber :: !Maybe !Text
  , _lastupdated :: !Maybe !Day
  , _officeid :: !Maybe !Int
  , _individualdesignationid :: !Maybe !Int
  , _orgcode :: !Maybe !Text
  , _sourceid :: !Maybe !Unknown
  , _photourl :: !Maybe !Text
  , _photoalternateurl :: !Maybe !Text
} deriving (Generic, Show)

instance ToJSON Agent
instance ToJSON Agent

instance Recorder Agent where
    type Key Agent = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM agent WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM agent"

data Alternateurls = Alternateurls {
    _id :: !Int
  , _moreinformationurl :: !Maybe !Text
  , _maplinkurl :: !Maybe !Text
} deriving (Generic, Show)

instance ToJSON Alternateurls
instance ToJSON Alternateurls

instance Recorder Alternateurls where
    type Key Alternateurls = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM alternateurls WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM alternateurls"

data Attachmentstyle = Attachmentstyle {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Attachmentstyle
instance ToJSON Attachmentstyle

instance Recorder Attachmentstyle where
    type Key Attachmentstyle = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM attachmentstyle WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM attachmentstyle"

data Building = Building {
    _id :: !Int
  , _buildingtypeid :: !Maybe !Int
  , _fullbathtotal :: !Maybe !Int
  , _halfbathtotal :: !Maybe !Int
  , _bedroomsaboveground :: !Maybe !Int
  , _bedroomsbelowground :: !Maybe !Int
  , _basementfinishedpercentage :: !Maybe !Int
  , _hasbasement :: !Maybe !Int
  , _yearbuilt :: !Maybe !Int
  , _attachmentstyleid :: !Maybe !Int
  , _coolingtypeid :: !Maybe !Int
  , _exteriorfinishid :: !Maybe !Int
  , _fireplacetotal :: !Maybe !Int
  , _foundationtypeid :: !Maybe !Int
  , _heatingfuelid :: !Maybe !Int
  , _heatingtypeid :: !Maybe !Int
  , _roofmaterialid :: !Maybe !Int
  , _storiestotal :: !Maybe !Int
  , _sizefrontage :: !Maybe !Decimal
  , _sizedepth :: !Maybe !Decimal
  , _livingspace :: !Maybe !Int
  , _evaluation :: !Maybe !Int
} deriving (Generic, Show)

instance ToJSON Building
instance ToJSON Building

instance Recorder Building where
    type Key Building = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM building WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM building"

data Buildingparkings = Buildingparkings {
    _buildingid :: !Int
  , _parkingid :: !Int
} deriving (Generic, Show)

instance ToJSON Buildingparkings
instance ToJSON Buildingparkings

instance Recorder Buildingparkings where
    type Key Buildingparkings = Key (!Int, !Int)
    key a = Key (_buildingid a, _parkingid a)
    find conn Key (k1, k2) = do
        x:_ <- query conn "SELECT * FROM buildingparkings WHERE _buildingid = ? AND _parkingid = ?" (k1, k2)
        return x
    findAll conn = query_ conn "SELECT * FROM buildingparkings"

data Buildingtype = Buildingtype {
    _id :: !Int
  , _name :: !Text
  , _associatedpropertytypeids :: !Maybe !Text
} deriving (Generic, Show)

instance ToJSON Buildingtype
instance ToJSON Buildingtype

instance Recorder Buildingtype where
    type Key Buildingtype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM buildingtype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM buildingtype"

data City = City {
    _id :: !Int
  , _regionid :: !Int
  , _provincestatecode :: !Text
  , _name :: !Text
  , _latitude :: !Maybe !Decimal
  , _longitude :: !Maybe !Decimal
} deriving (Generic, Show)

instance ToJSON City
instance ToJSON City

instance Recorder City where
    type Key City = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM city WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM city"

data Coolingtype = Coolingtype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Coolingtype
instance ToJSON Coolingtype

instance Recorder Coolingtype where
    type Key Coolingtype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM coolingtype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM coolingtype"

data Country = Country {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Country
instance ToJSON Country

instance Recorder Country where
    type Key Country = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM country WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM country"

data Documenttype = Documenttype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Documenttype
instance ToJSON Documenttype

instance Recorder Documenttype where
    type Key Documenttype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM documenttype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM documenttype"

data Exteriorfinish = Exteriorfinish {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Exteriorfinish
instance ToJSON Exteriorfinish

instance Recorder Exteriorfinish where
    type Key Exteriorfinish = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM exteriorfinish WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM exteriorfinish"

data Flooringtype = Flooringtype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Flooringtype
instance ToJSON Flooringtype

instance Recorder Flooringtype where
    type Key Flooringtype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM flooringtype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM flooringtype"

data Foundationtype = Foundationtype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Foundationtype
instance ToJSON Foundationtype

instance Recorder Foundationtype where
    type Key Foundationtype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM foundationtype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM foundationtype"

data Heatingfuel = Heatingfuel {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Heatingfuel
instance ToJSON Heatingfuel

instance Recorder Heatingfuel where
    type Key Heatingfuel = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM heatingfuel WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM heatingfuel"

data Heatingtype = Heatingtype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Heatingtype
instance ToJSON Heatingtype

instance Recorder Heatingtype where
    type Key Heatingtype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM heatingtype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM heatingtype"

data Individualdesignation = Individualdesignation {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Individualdesignation
instance ToJSON Individualdesignation

instance Recorder Individualdesignation where
    type Key Individualdesignation = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM individualdesignation WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM individualdesignation"

data Land = Land {
    _id :: !Int
  , _sizefrontage :: !Maybe !Decimal
  , _sizedepth :: !Maybe !Decimal
  , _area :: !Maybe !Int
  , _evaluation :: !Maybe !Int
  , _sewertypeid :: !Maybe !Int
} deriving (Generic, Show)

instance ToJSON Land
instance ToJSON Land

instance Recorder Land where
    type Key Land = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM land WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM land"

data Listingtype = Listingtype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Listingtype
instance ToJSON Listingtype

instance Recorder Listingtype where
    type Key Listingtype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM listingtype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM listingtype"

data Log = Log {
    _t :: !Maybe !UTCTime
  , _comment :: !Maybe !Text
} deriving (Generic, Show)

instance ToJSON Log
instance ToJSON Log

instance Recorder Log where
    type Key Log = Key ()
    key a = Key ()
    find conn Key () = do
        x:_ <- query conn "SELECT * FROM log WHERE " ()
        return x
    findAll conn = query_ conn "SELECT * FROM log"

data Metadata = Metadata {
    _id :: !Int
  , _attributename :: !Text
  , _value :: !Maybe !Text
} deriving (Generic, Show)

instance ToJSON Metadata
instance ToJSON Metadata

instance Recorder Metadata where
    type Key Metadata = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM metadata WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM metadata"

data Office = Office {
    _id :: !Int
  , _name :: !Maybe !Text
  , _email :: !Maybe !Text
  , _phone :: !Maybe !Text
  , _fax :: !Maybe !Text
  , _website :: !Maybe !Text
  , _lastupdated :: !Maybe !Day
  , _addressid :: !Maybe !Int
  , _orgcode :: !Maybe !Text
  , _sourceid :: !Maybe !Text
} deriving (Generic, Show)

instance ToJSON Office
instance ToJSON Office

instance Recorder Office where
    type Key Office = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM office WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM office"

data Organization = Organization {
    _code :: !Text
  , _name :: !Text
  , _organizationtypeid :: !Maybe !Int
} deriving (Generic, Show)

instance ToJSON Organization
instance ToJSON Organization

instance Recorder Organization where
    type Key Organization = Key (!Text)
    key a = Key (_code a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM organization WHERE _code = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM organization"

data Organizationtype = Organizationtype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Organizationtype
instance ToJSON Organizationtype

instance Recorder Organizationtype where
    type Key Organizationtype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM organizationtype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM organizationtype"

data Parking = Parking {
    _id :: !Int
  , _parkingtypeid :: !Maybe !Int
  , _spaces :: !Maybe !Int
} deriving (Generic, Show)

instance ToJSON Parking
instance ToJSON Parking

instance Recorder Parking where
    type Key Parking = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM parking WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM parking"

data Parkingtype = Parkingtype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Parkingtype
instance ToJSON Parkingtype

instance Recorder Parkingtype where
    type Key Parkingtype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM parkingtype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM parkingtype"

data Person = Person {
    _id :: !Int
  , _name :: !Text
  , _birthday :: !Maybe !UTCTime
} deriving (Generic, Show)

instance ToJSON Person
instance ToJSON Person

instance Recorder Person where
    type Key Person = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM person WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM person"

data Pooltype = Pooltype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Pooltype
instance ToJSON Pooltype

instance Recorder Pooltype where
    type Key Pooltype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM pooltype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM pooltype"

data Property = Property {
    _id :: !Int
  , _lastupdated :: !Maybe !Day
  , _price :: !Maybe !Int
  , _previousprice :: !Maybe !Int
  , _pricelastupdated :: !Maybe !Day
  , _addressid :: !Maybe !Int
  , _listingtypeid :: !Maybe !Int
  , _propertytypeid :: !Maybe !Int
  , _sourcecode :: !Maybe !Text
  , _sourceid :: !Maybe !Int
} deriving (Generic, Show)

instance ToJSON Property
instance ToJSON Property

instance Recorder Property where
    type Key Property = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM property WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM property"

data Propertyagents = Propertyagents {
    _propertyid :: !Int
  , _agentid :: !Int
} deriving (Generic, Show)

instance ToJSON Propertyagents
instance ToJSON Propertyagents

instance Recorder Propertyagents where
    type Key Propertyagents = Key (!Int, !Int)
    key a = Key (_propertyid a, _agentid a)
    find conn Key (k1, k2) = do
        x:_ <- query conn "SELECT * FROM propertyagents WHERE _propertyid = ? AND _agentid = ?" (k1, k2)
        return x
    findAll conn = query_ conn "SELECT * FROM propertyagents"

data Propertydetails = Propertydetails {
    _id :: !Int
  , _evaluation :: !Maybe !Int
  , _totaltax :: !Maybe !Int
  , _schooltax :: !Maybe !Int
  , _municipaltax :: !Maybe !Int
  , _condofees :: !Maybe !Int
  , _pooltypeid :: !Maybe !Int
  , _description :: !Maybe !Text
  , _descriptionfr :: !Maybe !Text
  , _viewtypeid :: !Maybe !Int
  , _additionalinfo :: !Maybe !Text
  , _additionalinfofr :: !Maybe !Text
  , _zoningtypeid :: !Maybe !Int
  , _representationtypeid :: !Maybe !Int
} deriving (Generic, Show)

instance ToJSON Propertydetails
instance ToJSON Propertydetails

instance Recorder Propertydetails where
    type Key Propertydetails = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM propertydetails WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM propertydetails"

data Propertyphoto = Propertyphoto {
    _id :: !Int
  , _name :: !Maybe !Text
  , _sequenceid :: !Maybe !Int
  , _propertydetailsid :: !Int
  , _url :: !Maybe !Text
  , _alternateurl :: !Maybe !Text
} deriving (Generic, Show)

instance ToJSON Propertyphoto
instance ToJSON Propertyphoto

instance Recorder Propertyphoto where
    type Key Propertyphoto = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM propertyphoto WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM propertyphoto"

data Propertytype = Propertytype {
    _id :: !Int
  , _name :: !Maybe !Text
} deriving (Generic, Show)

instance ToJSON Propertytype
instance ToJSON Propertytype

instance Recorder Propertytype where
    type Key Propertytype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM propertytype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM propertytype"

data Provincestate = Provincestate {
    _code :: !Text
  , _countryid :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Provincestate
instance ToJSON Provincestate

instance Recorder Provincestate where
    type Key Provincestate = Key (!Text)
    key a = Key (_code a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM provincestate WHERE _code = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM provincestate"

data Region = Region {
    _id :: !Int
  , _provincestatecode :: !Text
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Region
instance ToJSON Region

instance Recorder Region where
    type Key Region = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM region WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM region"

data Representationtype = Representationtype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Representationtype
instance ToJSON Representationtype

instance Recorder Representationtype where
    type Key Representationtype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM representationtype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM representationtype"

data Roofmaterial = Roofmaterial {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Roofmaterial
instance ToJSON Roofmaterial

instance Recorder Roofmaterial where
    type Key Roofmaterial = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM roofmaterial WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM roofmaterial"

data Room = Room {
    _id :: !Int
  , _buildingid :: !Maybe !Int
  , _roomtypeid :: !Maybe !Int
  , _flooringtypeid :: !Maybe !Int
  , _widthsize :: !Maybe !Decimal
  , _lengthsize :: !Maybe !Decimal
  , _levelnumber :: !Maybe !Int
} deriving (Generic, Show)

instance ToJSON Room
instance ToJSON Room

instance Recorder Room where
    type Key Room = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM room WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM room"

data Roomtype = Roomtype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Roomtype
instance ToJSON Roomtype

instance Recorder Roomtype where
    type Key Roomtype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM roomtype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM roomtype"

data Searchproperty = Searchproperty {
    _id :: !Int
  , _lastupdated :: !Maybe !Day
  , _price :: !Maybe !Int
  , _previousprice :: !Maybe !Int
  , _pricelastupdated :: !Maybe !Day
  , _streetaddress :: !Maybe !Text
  , _apartmentnumber :: !Maybe !Text
  , _cityid :: !Maybe !Int
  , _cityname :: !Maybe !Text
  , _subdivisionid :: !Maybe !Int
  , _subdivisionname :: !Maybe !Text
  , _provincestatecode :: !Maybe !Text
  , _countryid :: !Maybe !Int
  , _countryname :: !Maybe !Text
  , _postalcode :: !Maybe !Text
  , _latitude :: !Maybe !Decimal
  , _longitude :: !Maybe !Decimal
  , _listingtypeid :: !Maybe !Int
  , _listingtypename :: !Maybe !Text
  , _propertytypeid :: !Maybe !Int
  , _propertytypename :: !Maybe !Text
  , _buildingtypeid :: !Maybe !Int
  , _buildingtypename :: !Maybe !Text
  , _bedroomsaboveground :: !Maybe !Int
  , _bedroomsbelowground :: !Maybe !Int
  , _fullbathtotal :: !Maybe !Int
  , _halfbathtotal :: !Maybe !Int
  , _landarea :: !Maybe !Int
  , _livingspace :: !Maybe !Int
  , _thumbnailurl :: !Maybe !Text
  , _sourceid :: !Maybe !Int
} deriving (Generic, Show)

instance ToJSON Searchproperty
instance ToJSON Searchproperty

instance Recorder Searchproperty where
    type Key Searchproperty = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM searchproperty WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM searchproperty"

data Sewertype = Sewertype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Sewertype
instance ToJSON Sewertype

instance Recorder Sewertype where
    type Key Sewertype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM sewertype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM sewertype"

data Source = Source {
    _code :: !Text
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Source
instance ToJSON Source

instance Recorder Source where
    type Key Source = Key (!Text)
    key a = Key (_code a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM source WHERE _code = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM source"

data Subdivision = Subdivision {
    _id :: !Int
  , _cityid :: !Int
  , _name :: !Text
  , _latitude :: !Maybe !Decimal
  , _longitude :: !Maybe !Decimal
} deriving (Generic, Show)

instance ToJSON Subdivision
instance ToJSON Subdivision

instance Recorder Subdivision where
    type Key Subdivision = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM subdivision WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM subdivision"

data Viewtype = Viewtype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Viewtype
instance ToJSON Viewtype

instance Recorder Viewtype where
    type Key Viewtype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM viewtype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM viewtype"

data Zoningtype = Zoningtype {
    _id :: !Int
  , _name :: !Text
} deriving (Generic, Show)

instance ToJSON Zoningtype
instance ToJSON Zoningtype

instance Recorder Zoningtype where
    type Key Zoningtype = Key (!Int)
    key a = Key (_id a)
    find conn Key (k1) = do
        x:_ <- query conn "SELECT * FROM zoningtype WHERE _id = ?" (k1)
        return x
    findAll conn = query_ conn "SELECT * FROM zoningtype"

