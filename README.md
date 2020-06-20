EDC Configurator
================

> Web gui for create/manage EDC sets

## Maintenance and self-notes

### SSL cert
> https://medium.com/@marcmathijssen/add-ssl-to-azure-web-app-using-letsencrypt-9125c3fdfb03
- `sudo certbot certonly --preferred-challenges http -d myedc.cz --manual`
- To success the acme challenge, use an in-app route and pass a value to db (*todo*)
- `sudo openssl pkcs12 -export -out ./myedc.pfx -inkey /etc/letsencrypt/live/myedc.cz/privkey.pem -in /etc/letsencrypt/live/myedc.cz/cert.pem`

---
## SAFE Template

> Created by `dotnet new SAFE --layout fulma-admin --communication remoting --deploy azure --js-deps npm` (_originally with `--deploy docker`_)

This template can be used to generate a full-stack web application using the [SAFE Stack](https://safe-stack.github.io/). It was created using the dotnet [SAFE Template](https://safe-stack.github.io/docs/template-overview/). If you want to learn more about the template why not start with the [quick start](https://safe-stack.github.io/docs/quickstart/) guide?

### Install pre-requisites

You'll need to install the following pre-requisites in order to build SAFE applications

* The [.NET Core SDK](https://www.microsoft.com/net/download)
* The [Yarn](https://yarnpkg.com/lang/en/docs/install/) package manager (you can also use `npm` but the usage of `yarn` is encouraged).
* [Node LTS](https://nodejs.org/en/download/) installed for the front end components.
* If you're running on OSX or Linux, you'll also need to install [Mono](https://www.mono-project.com/docs/getting-started/install/).

### Work with the application

Before you run the project **for the first time only** you should install its local tools with this command:

```bash
dotnet tool restore
```


To concurrently run the server and the client components in watch mode use the following command:

```bash
dotnet fake build -t run
```


You can use the included `Dockerfile` and `build.fsx` script to deploy your application as Docker container. You can find more regarding this topic in the [official template documentation](https://safe-stack.github.io/docs/template-docker/).

You can use the included `arm-template.json` file and `build.fsx` script to deploy you application as an Azure Web App. Consult the [official template documentation](https://safe-stack.github.io/docs/template-appservice/) to learn more.

### SAFE Stack Documentation

You will find more documentation about the used F# components at the following places:

* [Saturn](https://saturnframework.org/docs/)
* [Fable](https://fable.io/docs/)
* [Elmish](https://elmish.github.io/elmish/)
* [Fable.Remoting](https://zaid-ajaj.github.io/Fable.Remoting/)
* [Fulma](https://fulma.github.io/Fulma/)

If you want to know more about the full Azure Stack and all of it's components (including Azure) visit the official [SAFE documentation](https://safe-stack.github.io/docs/).
