namespace MF.EDC.Command

open ErrorHandling

type Command<'Data, 'Success, 'Error> = 'Data -> AsyncResult<'Success, 'Error>    // todo - this might be IO monad someday in the future - which would be AsyncResult + Reader monad for DI
