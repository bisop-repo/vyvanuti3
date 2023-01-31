#ifndef TOOLTOOLS_HPP
#define TOOLTOOLS_HPP

#include "orpp/csv.hpp"
#include <limits.h>
#include <time.h>
//#include "districts.hpp"

using namespace std;
using namespace orpp;


enum cohorts { c0, c20, c65, c80, numcohorts};


cohorts v2cohort(unsigned v)
{
    if(v < 20)
        return c0;
    else
    {
        if(v<65)
            return c20;
        else
        {
            if(v < 80)
                return c65;
            else
            {
                if(v < 105)
                    return c80;
                else
                    return numcohorts;
            }
        }
    }
};

using reldate = int;

constexpr reldate maxreldate = numeric_limits<reldate>::max();


int date2int(const string s)
{
    struct tm ti;
    ti.tm_sec=0;
    ti.tm_min=0;
    ti.tm_hour=0;
    ti.tm_mday= stoi(s.substr(8,2));
    ti.tm_mon = stoi(s.substr(5,2))-1;
    ti.tm_year=stoi(s.substr(0,4))-1900;

    time_t res = mktime(&ti);
    int di = round((res + 86400 *0.5) / 86400);
    return di;
}


string int2date(reldate date)
{
    time_t t = date * 86400;
    tm* ti = localtime(&t);
    unsigned y = ti->tm_year + 1900;
    unsigned m = ti->tm_mon+1;
    unsigned d = ti->tm_mday;

    ostringstream o;
    o << y << "-";
    if(m<10)
        o << "0";
    o << m << "-";
    if(d<10)
        o << "0";
    o << d;
    return o.str();
}



reldate date2int(const string s, reldate zerodate, reldate lastdate, bool& error, string& errormessage)
{
    error = true;
    string errorstr;
    reldate di;
    if(s=="")
        errorstr = "empty date";
    else if(s.size() != 10)
        errorstr = "date too long";
    else
    {
        di = date2int(s);
        if(di < zerodate)
            errorstr = "date too early";
        else if(di > lastdate)
            errorstr = "date too late";
        else
            error = false;
    }
    if(error)
    {
        ostringstream o;
        o << "Error in conversion of '" << s << "' to date: " << errorstr;
        o << "(min=" << int2date(zerodate) << ", max=" << int2date(lastdate) << ")";
        errormessage = o.str();
        return maxreldate;
    }
    else
    {
        errormessage = "";
        return di;
    }
}

string threedigits(unsigned i)
{
    if(i>999)
        throw "four digit integer!";
    ostringstream s;
    if(i<10)
        s << "0";
    if(i<100)
        s << "0";
    s << i;
    return s.str();
}


#endif // TOOLTOOLS_HPP
