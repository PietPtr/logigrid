# logigrid

## Optie 1: FPGA-like.

Gegeven is een klein ongeconfigureerd _simpel_ FPGA grid. Tiles zijn 1 2-LUT, een register, en een regmux. De 4 inputs van de tile zitten links via een switch box aan een routing network. De 1 output zit rechts aan het network. Op de intersections van het routing network liggen switch boxes.

Een player kan met WASD bestuurd worden, en met E acties uitvoeren op de dichtsbijzijnde LUT, register, of switch box. De acties openen menus waarmee de dingen geconfigureerd kunnen worden.

Met Enter kan in en uitgezoomd worden tussen LUT-view en chip-view. In chip-view kan je selecteren wat er op de inputs staat, en de clock beheren (staat in eerste instantie uit).

Gamification: doe berekenjobs voor tokens, daarmee:
- speel je meer tiles vrij
- kan de arity van specifieke LUTs verhoogd worden
- kan de clock gestart of versneld worden.
- copy paste tool
    - per LUT
    - per tile
    - per tile selectie
- density van routing network
- verschillende switch box topologies

(Creative mode: gigantische FPGA alvast vrijgespeeld)

## Optie 2: ASIC-like.

Grid waar je cells neerzet en kabels naartoe aanlegt. Je mag cells abstracten.

# TODO's

[x] dt propageren
[x] svg renderen
    [x] LUT rendering
    [x] io switch drawing
    [x] routing switch drawing
[ ] toolie om nets en switches uit SVGs te extracten en daar nieuwe SVGs met actieve edges van te maken?
[ ] configurations renderen
    [x] LUT configuration
    [ ] ioswitch configuration
    [ ] router configuration
    [ ] mux state
    [ ] register state
    [x] tile netstates
    [ ] io switch netstate
    [ ] router netstate
    [x] input state
    [x] input netstate
    [ ] output state
    [ ] output netstate
[ ] simulatie
    [ ] tile logic
    [ ] netstate propagatie op basis van configuratie
[ ] acties
[ ] lopen wat natuurlijker met wat kleine velocities enzo