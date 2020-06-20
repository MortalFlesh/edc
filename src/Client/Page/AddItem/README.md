Add item
========

## Fields

### Type
> ItemType - SelectBox

### SubType
> ItemSubType - SelectBox

---

### Name
> Required string (validate by NewItem)

### Note
> Optional string (validate by NewItem)

### Color
> Checkbox "with color" -> Optional String <ColorPicker -> string, None>

### Tags
> Tag list, (validate by AsyncResult list) 
- on [",", " ", ";", "tab"] -> validate current -> tags.Add(result)
- input [ (Ok "tag"); (Ok "tag2"); (Error "tagWithError") ] |> AsyncResult.list // Tags [ "tag"; "tag2" ]

### Links
> Link list, (validate each link |> Validation.sequence) // error per index
- on notEmptyLink -> add new input (till max)

### Price
> float + currency
- input [ float ] + currency selectBox(default by lang)

### Size
> Checkbox "with size"

### OwnershipStatus
> Required string SelectBox

---
## Product
> Checkbox "add product info", when checked -> product info should be filled - separate smart sub-component with own state

### ProductName
>  = None

### ProductManufacturer
>  = None

### ProductPrice
>  = None

### ProductEan
>  = None

### ProductLinks
>  = []

---
### Gallery
> Checkbox "add gallery", when checked -> gallery could be filled - separate smart sub-component with own state
