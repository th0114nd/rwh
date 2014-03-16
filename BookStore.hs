type CustomerID = Int
type ReviewBody = String
type BookRecord = (BookInfo, BookReview)
data BookInfo = Book Int String [String] deriving (Show)
data MagazineInfo = Magazine Int String [String] deriving (Show)
data BookReview = BookReview BookInfo CustomerID ReviewBody
type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)


myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird",
                                                      "Oege de Moor"]
bookID (Book id title authors) = id
bookTitle (Book id title authors) = title
bookAuthors (Book id title authors) = authors

data Customer = Customer {
    customerID :: CustomerID,
    customerName :: String,
    customerAddress :: Address
} deriving (Show)

customer1 = Customer 2718 "J.R. Hacker"
            ["255 Syntax Ct", "Milpitas, CA 95134", "USA"]

customer2 = Customer {
    customerID = 271828
    , customerAddress = ["blah", "hermph", "Foo"]
    , customerName ="Jane Q. Citizen"
}

                                                      
