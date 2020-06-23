<?php declare(strict_types=1);

namespace MF\Edc\Component;

class LoginPageComponent extends AbstractEdcComponent
{
    public function login(string $user, string $password): void
    {
        $this->sendKeysSlower('Login-Username', $user);
        $this->sendKeysSlower('Login-Password', $password);
        $this->findByCss('.button.is-primary')->click();

        $this->waitForNotification('is-success');
    }

    public function assertLoggedIn(string $expectedUser): void
    {
        $this->tc->assertSame($expectedUser, $this->getTextByCss('.navbar-end .navbar-item.is-active'));
    }

    public function logout(): void
    {
        $this->hoverByCss('.navbar-end .navbar-item.is-hoverable.has-dropdown');
        $this->milliSleep(50);

        $this->clickOnLink('Log out');
        $this->milliSleep(300);
    }
}
