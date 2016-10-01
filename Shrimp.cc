class Shrimp : public Box {
    public:
        Shrimp(string date) {
            isOpen = false;
            maxItems = 50;
            itemsLeft = maxItems;
            expirationDate = date;
        };
};
