#ifndef PKG_RMINIZINC_H
#define PKG_RMINIZINC_H

std::string deleteItem(int itemNo,
                std::string modelString = "", 
                std::string mznpath = "",
                std::string  modelStringName = "deleteItem.mzn",
                bool updateMZN = false);

#endif