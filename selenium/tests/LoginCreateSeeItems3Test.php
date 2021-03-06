<?php declare(strict_types=1);

namespace MF\Edc;

class LoginCreateSeeItems3Test extends AbstractLoginCreateSeeItems
{
    /**
     * @dataProvider provideUsername
     */
    public function testShouldLoginCreateItemsAndSeeThem(string $username): void
    {
        $this->shouldLoginCreateItemsAndSeeThem($username);
    }
}
