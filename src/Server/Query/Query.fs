namespace MF.EDC.Query

open ErrorHandling

type Query<'Success, 'Error> = AsyncResult<'Success, 'Error>    // todo - this might be IO monad someday in the future - which would be AsyncResult + Reader monad for DI
