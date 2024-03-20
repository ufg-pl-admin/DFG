# System Deweloperskiego Funduszu Gwarancyjnego

Moduł Map
-------
W ramach Systemu DFG dostarczony został moduł prezentujący inwestycje deweloperskie w Polsce. Moduł składa się z biblioteki Javascript opartej o Leaflet, prezentującej inwestycje na mapie oraz z REST API.


### Biblioteka
Plik mapapp.js można osadzić w ramach dokumentu HTML przekazując odpowiednie parametry:
| Parametr | Typ     | Opis                       |
| :-------- | :------- | :-------------------------------- |
| `mapElement`      | `HTMLElement` | Element HTML, w którym mapa będzie wyświetlana |
| `options`      | `string` | Opcjonalny obiekt zawierający parametry |
| `options.mapAppUr`      | `string` | Adres URL do API mapowego |
| `options.center`      | `number[]` | Domyślny punkt centralny mapy podany jako para [szerokość, długość] |
| `options.zoom`      | `number` | Domyślne przybliżenie mapy |


### API
Adres API: https://public.api.ufg.pl/dfg/mapapp/portal/api/v1/investment

#### Pobierz wszystkie inwestycje

```http
  GET /all
```



#### Pobierz szczegóły inwestycji

```http
  GET /details/${id}
```

| Parametr | Typ     | Opis                       |
| :-------- | :------- | :-------------------------------- |
| `id`      | `string` | Identyfikator inwestycji |



Moduł Statystyk
-------
Moduł wytworzony w języku R, zawiera kod odpowiedzialny za generowanie ikonografii i wykresów prezentujących dane statystyczne. 


### Uruchomienie
W środowisku RStudio należy uruchomić plik `/src/run.R`

Po uruchomieniu dostępny jest Swagger zgodny z OAS3 dostępny pod adresem:
 `http://127.0.0.1:8080/__docs__/`


Licencja
-------

Kod udostępniony jest na licencji [MPL 2.0](https://www.mozilla.org/en-US/MPL/2.0/)


