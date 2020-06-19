<?php declare(strict_types=1);

namespace MF\Edc\Component;

use Facebook\WebDriver\Exception\NoSuchElementException;

class ItemsComponent extends AbstractEdcComponent
{
    public function countItems(): int
    {
        try {
            return $this->countByCss('table.is-hoverable.is-striped.is-fullwidth.is-bordered tbody tr');
        } catch (NoSuchElementException $e) {
            return 0;
        }
    }

    public function goToAddItem(): void
    {
        $this->findByCss('.button.is-success')->click();
        $this->milliSleep(500);
    }
}
