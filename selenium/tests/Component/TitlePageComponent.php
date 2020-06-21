<?php declare(strict_types=1);

namespace MF\Edc\Component;

class TitlePageComponent extends AbstractEdcComponent
{
    public function goToTitlePage(): void
    {
        $this->wd->get($this->baseUrl);
    }
}
