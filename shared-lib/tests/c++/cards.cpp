#include <iomanip>
#include <iostream>

#include "cards.hpp"

using namespace std;

extern const uint32_t DEVICEID;
extern const uint8_t DOOR;
extern bool result(string test, bool ok);

bool getCards(uhppoted &u) {
    try {
        auto N = u.get_cards(DEVICEID);
        bool ok = true;

        if (N != 39) {
            cout << "get-cards: incorrect card count - expected:" << 39 << ", got:" << N << endl;
            ok = false;
        }

        return result("get-cards", ok);
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}
