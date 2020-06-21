<?php declare(strict_types=1);

namespace MF\Edc\Component;

use Facebook\WebDriver\Remote\RemoteWebElement;
use Facebook\WebDriver\WebDriverAlert;
use Facebook\WebDriver\WebDriverBy;
use Facebook\WebDriver\WebDriverElement;
use Facebook\WebDriver\WebDriverExpectedCondition;
use Facebook\WebDriver\WebDriverSelect;
use Lmc\Steward\Component\AbstractComponent;
use MF\Edc\AbstractEdcTestCase;

abstract class AbstractEdcComponent extends AbstractComponent
{
    /** @var string */
    protected $baseUrl;
    /** @var string */
    protected $rootDir;

    public function __construct(AbstractEdcTestCase $tc)
    {
        parent::__construct($tc);

        $this->baseUrl = $tc->baseUrl;
        $this->rootDir = __DIR__ . '/../../';
    }

    public function getUrl(): string
    {
        return $this->wd->getCurrentURL();
    }

    public function getTitle(): string
    {
        return $this->wd->getTitle();
    }

    public function getH1(): string
    {
        return $this->findByCss('h1')->getText();
    }

    protected function getTextByCss(string $cssSelector): string
    {
        return $this->getText($this->findByCss($cssSelector));
    }

    protected function getText(WebDriverElement $element): string
    {
        return trim($element->getText());
    }

    protected function getTextById(string $id): string
    {
        return $this->getText($this->findById($id));
    }

    protected function getTextByXpath(string $xpath): string
    {
        return $this->getText($this->findByXpath($xpath));
    }

    protected function getTextsByCss(string $cssSelector): array
    {
        $elements = $this->findMultipleByCss($cssSelector);

        return $this->mapElementsToText($elements);
    }

    /**
     * @param RemoteWebElement[] $elements
     */
    protected function mapElementsToText(array $elements): array
    {
        return array_map(function (RemoteWebElement $element) {
            return $this->getText($element);
        }, $elements);
    }

    protected function countByCss(string $cssSelector): int
    {
        $items = $this->findMultipleByCss($cssSelector);

        return count($items);
    }

    protected function findByCssAndIndex(string $selector, int $index): ?RemoteWebElement
    {
        $elements = $this->findMultipleByCss($selector);

        return $elements[$index] ?? null;
    }

    protected function clickOnLink(string $partialLinkText, string $expectedPartialTitle = 'EDC'): void
    {
        $this->waitForPartialLinkText($partialLinkText)->click();
        $this->waitForPartialTitle($expectedPartialTitle);
    }

    protected function waitForCssAfterRefresh(string $selector)
    {
        return $this->wd->wait()->until(
            WebDriverExpectedCondition::refreshed(
                WebDriverExpectedCondition::visibilityOfElementLocated(WebDriverBy::cssSelector($selector))
            )
        );
    }

    protected function findSelect(string $selector): WebDriverSelect
    {
        return new WebDriverSelect($this->findByCss($selector));
    }

    protected function getSelectSelectedValue(string $selector): string
    {
        return $this->getText($this->findSelect($selector)->getFirstSelectedOption());
    }

    protected function findAlert(): WebDriverAlert
    {
        return $this->wd->switchTo()->alert();
    }

    protected function waitForNotification(string $type): RemoteWebElement
    {
        return $this->waitForCss(sprintf('.notification.%s', $type), true);
    }

    protected function sendKeysSlower(string $id, string $value, int $milliseconds = 100): void
    {
        $element = $this->findById($id);

        foreach (mb_str_split($value) as $char) {
            $element->sendKeys($char);
            $this->milliSleep($milliseconds);
        }
    }

    protected function milliSleep(int $milliseconds): void
    {
        usleep($milliseconds * 1000);
    }

    protected function hoverByCss(string $selector): void
    {
        $element = $this->findByCss($selector);
        $this->wd->getMouse()->mouseMove($element->getCoordinates());
    }
}
