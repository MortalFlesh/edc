namespace MF.EDC.Query

open ErrorHandling
open MF.EDC

[<RequireQualifiedAccess>]
type ItemsError =
    | Runtime of string

[<RequireQualifiedAccess>]
module ItemsError =
    let format = function
        | ItemsError.Runtime error -> error

type LoadItemsQuery = Query<ItemEntity list, ItemsError>

[<RequireQualifiedAccess>]
module ItemsQuery =
    let load: LoadItemsQuery = asyncResult {
        let items = [
            {
                Id = Id.create()
                Item = Item.Tool (Knife {
                    Common = {
                        Name = "Gerber Metolius Fixed"
                        Note = None; Color = None; Tags = []; Links = []; Price = None
                        Size = Some {
                            Weight = Some (Weight 170<Gram>)
                            Dimensions = Some { Height = 40<Milimeter>; Width = 25<Milimeter>; Length = 220<Milimeter> }
                        }
                        OwnershipStatus = Own
                        Product = Some {
                            Name = "Gerber Motelius"
                            Price = { Amount = 1200.; Currency = Czk }
                            Ean = None
                            Links = []
                        }
                        Gallery = None
                    }
                })
            }

            {
                Id = Id.create()
                Item = Item.Tool (Knife {
                    Common = {
                        Name = "Gerber Metolius Foldable"
                        Note = None; Color = None; Tags = []; Size = None
                        Links = [
                            Link "http://www.noze-nuz.com/gerber/G0009.php"
                        ]
                        Price = Some { Amount = 1200.; Currency = Czk }
                        OwnershipStatus = Wish
                        Product = Some {
                            Name = "Gerber Metolius Foldable"
                            Price = {
                                Amount = 1200.
                                Currency = Czk
                            }
                            Ean = None
                            Links = [
                                Link "https://moskito.cz/produkt/zaviraci-nuz-gerber-metolius-folder/"
                                Link "https://matum.cz/zaviraci-nuz-metolius-14298"
                            ]
                        }
                        Gallery = None
                    }
                })
            }
        ]

        return items
    }