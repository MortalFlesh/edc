<?php declare(strict_types=1);

namespace MF\Edc;

use Lmc\Steward\Test\AbstractTestCase;

abstract class AbstractEdcTestCase extends AbstractTestCase
{
    private const BASE_LOCAL_URL = 'http://192.168.1.98:8080';
    private const BASE_PUBLIC_URL = 'https://www.myedc.cz';
    private const BASE_DIRECT_URL = 'http://myedc.cz';

    private const BASE_URL = self::BASE_PUBLIC_URL;

    /** @var string */
    public $baseUrl;

    public function __construct($name = null, array $data = [], $dataName = '')
    {
        parent::__construct($name, $data, $dataName);

        $this->baseUrl = self::BASE_URL;
    }
}
