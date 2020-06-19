<?php declare(strict_types=1);

namespace MF\Edc\Component;

class LoginPageComponent extends AbstractEdcComponent
{
    public function goToLoginPage(): void
    {
        $this->wd->get($this->baseUrl . '/#/login');
    }

    public function login(string $user, string $password): void
    {
        $this->sendKeysSlower('Login-Username', $user);
        $this->sendKeysSlower('Login-Password', $password);
        $this->findByCss('a.button.is-success')->click();

        $this->waitForNotification('is-success');
    }

    public function assertLoggedIn(string $expectedUser): void
    {
        $this->tc->assertSame($expectedUser, $this->getTextByCss('.navbar-end .navbar-item.is-active'));
    }
}
