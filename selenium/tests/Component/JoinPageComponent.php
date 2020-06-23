<?php declare(strict_types=1);

namespace MF\Edc\Component;

class JoinPageComponent extends AbstractEdcComponent
{
    public function join(string $username, string $email, string $password): void
    {
        $this->sendKeysSlower('Join-Username', $username);
        $this->sendKeysSlower('Join-Email', $email);
        $this->sendKeysSlower('Join-Password', $password);
        $this->sendKeysSlower('Join-PasswordCheck', $password);
        $this->findByCss('.button.is-primary')->click();

        $this->waitForNotification('is-success');
    }
}
