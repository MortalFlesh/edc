<?php declare(strict_types=1);

namespace MF\Edc;

use Faker\Factory;
use Faker\Generator;
use MF\Edc\Component\AddItemComponent;
use MF\Edc\Component\ItemsComponent;
use MF\Edc\Component\JoinPageComponent;
use MF\Edc\Component\LoginPageComponent;
use MF\Edc\Component\MenuComponent;
use MF\Edc\Component\TitlePageComponent;

abstract class AbstractLoginCreateSeeItems extends AbstractEdcTestCase
{
    /** @var TitlePageComponent */
    private $titlePage;
    /** @var JoinPageComponent */
    private $joinPage;
    /** @var LoginPageComponent */
    private $loginPage;
    /** @var MenuComponent */
    private $menu;
    /** @var ItemsComponent */
    private $items;
    /** @var AddItemComponent */
    private $addItem;
    /** @var Generator */
    private $faker;

    /**
     * @before
     */
    public function init(): void
    {
        $this->titlePage = new TitlePageComponent($this);
        $this->joinPage = new JoinPageComponent($this);
        $this->loginPage = new LoginPageComponent($this);
        $this->menu = new MenuComponent($this);
        $this->items = new ItemsComponent($this);
        $this->addItem = new AddItemComponent($this);

        $this->faker = Factory::create();
    }

    public function shouldLoginCreateItemsAndSeeThem(string $email): void
    {
        [$username] = explode('@', $email);
        $password = $this->faker->password;

        $this->debug('Go to title page');
        $this->titlePage->goToTitlePage();

        $this->debug('Join to page with %s', $email);
        $this->menu->goTo('Join');

        $this->joinPage->join($username, $email, $password);
        $this->loginPage->assertLoggedIn($username);
        $this->loginPage->logout();

        $usernameOrEmail = $this->faker->randomElement([$username, $email]);

        $this->debug('Login to page with %s', $usernameOrEmail);
        $this->menu->goTo('Log in');

        $this->loginPage->login($usernameOrEmail, $password);
        $this->loginPage->assertLoggedIn($username);

        $this->debug('Go to items');
        $this->menu->goTo('Items');
        $this->menu->assertPage('Items');

        $startItemsCount = $this->items->countItems();
        $this->debug('Current user has %d items.', $startItemsCount);

        $currentItemsCount = $startItemsCount;
        foreach ($this->provideItems() as $item) {
            $this->debug('Go to add-item and create new item (%s)', $item['name']);
            $this->items->goToAddItem();

            $this->debug('Fill new item');
            $this->addItem->fillItem($item);
            $this->addItem->save();

            $this->debug('Wait for new item ...');
            $this->waitForLinkText($item['name']);

            $this->debug('Check new item (%s)', $item['name']);
            $newItemsCount = $this->items->countItems();
            $this->assertSame($currentItemsCount + 1, $newItemsCount);
            $currentItemsCount = $newItemsCount;

            usleep(500 * 1000);
        }
    }

    public function provideItems(): iterable
    {
        $i = 0;
        $maxCount = $this->rollDice() * $this->rollDice();
        $this->debug('Generating %d items.', $maxCount);

        while ($i++ <= $maxCount) {
            $this->debug('Generate item %d ...', $i);

            yield [
                'name' => $this->value(trim($this->faker->sentence($this->oneToThree()), '.'), 3),
                'note' => $this->value($this->rollDice() > 4 ? $this->faker->sentence . ' ' : '', 5),
                'tags' => trim($this->faker->sentence($this->rollDice()), '.'),
                'ownership' => $this->faker->randomElement([
                    'Own',
                    'Wish',
                    'Maybe',
                    'Idea',
                    'ToBuy',
                    'ToSell',
                    'Ordered',
                ]),
                'product' => [
                    'name' => trim($this->faker->sentence($this->oneToThree()), '.'),
                    'manufacturer' => $this->value(trim($this->faker->sentence($this->oneToThree()), '.'), 2),
                    'ean' => $this->rollDice() > 3 ? $this->faker->ean13 : '',
                ],
            ];
        }
    }

    public function provideUsername(): iterable
    {
        $i = 0;
        $max = 10;
        $faker = Factory::create();

        while ($i++ < $max) {
            $email = $faker->email;
            yield $email => [$email];
        }
    }

    private function rollDice(): int
    {
        return random_int(0, 6);
    }

    private function oneToThree(): int
    {
        return random_int(1, 3);
    }

    private function value(string $value, int $minLength)
    {
        return mb_strlen($value) >= $minLength
            ? $value
            : $value . str_repeat('a', $minLength);
    }
}
