<?php declare(strict_types=1);

namespace MF\Edc\Component;

class MenuComponent extends AbstractEdcComponent
{
    public function goTo(string $page): void
    {
        $this->clickOnLink($page, 'EDC');
        $this->milliSleep(500);
    }

    public function assertPage(string $expectedPage): void
    {
        $this->tc->assertSame($expectedPage, $this->getTextByCss('.navbar-menu .navbar-item.is-active'));
    }
}
