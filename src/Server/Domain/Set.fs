namespace MF.EDC

type EDCSet = {
    Id: Id
    Name: string option
    Description: string option
    Inventory: ContainerEntity list
}
