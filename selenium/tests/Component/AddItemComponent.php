<?php declare(strict_types=1);

namespace MF\Edc\Component;

class AddItemComponent extends AbstractEdcComponent
{
    public function fillItem(array $item): void
    {
        [
            'name' => $itemName,
            'note' => $note,
            'tags' => $tags,
            'ownership' => $ownership,
            'product' => $product,
        ] = $item;

        [
            'name' => $productName,
            'manufacturer' => $productManufacturer,
            'ean' => $ean,
        ] = $product;

        $this->sendKeysSlower('AddItem-Name', $itemName);
        $this->sendKeysSlower('AddItem-Note', $note);
        $this->sendKeysSlower('AddItem-Tags', $tags, 1000);
        $this->sendKeysSlower('AddItem-Ownership', $ownership);
        $this->sendKeysSlower('Product-Name', $productName);
        $this->sendKeysSlower('Product-Manufacturer', $productManufacturer);
        $this->sendKeysSlower('Product-Ean', $ean);

        $this->milliSleep(300);
    }

    public function save(): void
    {
        $this->findByCss('.button.is-primary')->click();
        sleep(1);
    }
}
