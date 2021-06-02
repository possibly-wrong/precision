#ifndef MATH_INTEGER_H
#define MATH_INTEGER_H

#include "math_Unsigned.h"

namespace math
{
    class Integer
    {
    public:
        Integer(std::int32_t u = 0) :
            sign((u > 0) - (u < 0)),
            mag(u < 0 ? -u : u)
        {
            // empty
        }

        Integer(const Unsigned& u) :
            sign(u == 0 ? 0 : 1),
            mag(u)
        {
            // empty
        }

        Integer(const std::string& s) :
            sign(0),
            mag()
        {
            std::istringstream iss(s);
            iss >> *this;
            if (iss.fail() || !iss.eof())
            {
                throw std::runtime_error("Error: Integer::string");
            }
        }

        Integer(const Integer& copy) :
            sign(copy.sign),
            mag(copy.mag)
        {
            // empty
        }

        Integer& operator= (const Integer& rhs)
        {
            sign = rhs.sign;
            mag = rhs.mag;
            return *this;
        }

        Integer operator+ () const
        {
            return *this;
        }

        Integer operator- () const
        {
            Integer w(*this);
            w.sign = -sign;
            return w;
        }

        Integer operator++ (int)
        {
            Integer w(*this);
            ++(*this);
            return w;
        }

        Integer& operator++ ()
        {
            if (sign < 0)
            {
                if (--mag == 0)
                {
                    sign = 0;
                }
            }
            else
            {
                sign = 1;
                ++mag;
            }
            return *this;
        }

        Integer operator-- (int)
        {
            Integer w(*this);
            --(*this);
            return w;
        }

        Integer& operator-- ()
        {
            if (sign > 0)
            {
                if (--mag == 0)
                {
                    sign = 0;
                }
            }
            else
            {
                sign = -1;
                ++mag;
            }
            return *this;
        }

        friend Integer operator+ (Integer u, const Integer& v)
        {
            u += v;
            return u;
        }

        Integer& operator+= (const Integer& rhs)
        {
            if (sign == 0)
            {
                *this = rhs;
            }
            else if (rhs.sign != 0)
            {
                if (sign == rhs.sign)
                {
                    mag += rhs.mag;
                }
                else if (rhs.mag < mag)
                {
                    mag -= rhs.mag;
                }
                else if (rhs.mag == mag)
                {
                    sign = 0;
                    mag = 0;
                }
                else
                {
                    sign = rhs.sign;
                    mag = rhs.mag - mag;
                }
            }
            return *this;
        }

        friend Integer operator- (const Integer& u, const Integer& v)
        {
            return u + (-v);
        }

        Integer& operator-= (const Integer& rhs)
        {
            *this += -rhs;
            return *this;
        }

        friend Integer operator* (Integer u, const Integer& v)
        {
            u *= v;
            return u;
        }

        Integer& operator*= (const Integer& rhs)
        {
            sign *= rhs.sign;
            mag *= rhs.mag;
            return *this;
        }

        friend Integer operator/ (Integer u, const Integer& v)
        {
            u /= v;
            return u;
        }

        Integer& operator/= (const Integer& rhs)
        {
            sign *= rhs.sign;
            mag /= rhs.mag;
            if (mag == 0)
            {
                sign = 0;
            }
            return *this;
        }

        friend Integer operator% (Integer u, const Integer& v)
        {
            u %= v;
            return u;
        }

        Integer& operator%= (const Integer& rhs)
        {
            mag %= rhs.mag;
            if (mag == 0)
            {
                sign = 0;
            }
            return *this;
        }

        friend Integer operator<< (Integer u, size_t v)
        {
            u <<= v;
            return u;
        }

        Integer& operator<<= (size_t rhs)
        {
            mag <<= rhs;
            return *this;
        }

        friend Integer operator>> (Integer u, size_t v)
        {
            u >>= v;
            return u;
        }

        Integer& operator>>= (size_t rhs)
        {
            mag >>= rhs;
            if (mag == 0)
            {
                sign = 0;
            }
            return *this;
        }

        Integer operator~ () const
        {
            Integer w = -(*this);
            return --w;
        }

        friend Integer operator& (Integer u, const Integer& v)
        {
            u &= v;
            return u;
        }

        Integer& operator&= (Integer rhs)
        {
            if (sign < 0 && rhs.sign < 0)
            {
                // -u & -v == -((u-1) | (v-1) + 1)
                --mag;
                mag |= --rhs.mag;
                ++mag;
            }
            else
            {
                if (sign > 0 && rhs.sign < 0)
                {
                    // u & -v == u & ~(v-1)
                    mag = mag.and_not(--rhs.mag);
                }
                else if (sign < 0 && rhs.sign > 0)
                {
                    --mag;
                    mag = rhs.mag.and_not(mag);
                }
                else
                {
                    mag &= rhs.mag;
                }
                sign = (mag == 0 ? 0 : 1);
            }
            return *this;
        }

        friend Integer operator^ (Integer u, const Integer& v)
        {
            u ^= v;
            return u;
        }

        Integer& operator^= (Integer rhs)
        {
            if (sign < 0 && rhs.sign < 0)
            {
                // -u ^ -v == (u-1) ^ (v-1)
                --mag;
                mag ^= --rhs.mag;
                sign = (mag == 0 ? 0 : 1);
            }
            else if (sign > 0 && rhs.sign < 0)
            {
                // u ^ -v == -(u ^ (v-1) + 1)
                sign = -1;
                mag ^= --rhs.mag;
                ++mag;
            }
            else if (sign < 0 && rhs.sign > 0)
            {
                --mag;
                mag ^= rhs.mag;
                ++mag;
            }
            else
            {
                sign |= rhs.sign;
                mag ^= rhs.mag;
                if (mag == 0)
                {
                    sign = 0;
                }
            }
            return *this;
        }

        friend Integer operator| (Integer u, const Integer& v)
        {
            u |= v;
            return u;
        }

        Integer& operator|= (Integer rhs)
        {
            if (sign < 0 && rhs.sign < 0)
            {
                // -u | -v == -((u-1) & (v-1) + 1)
                --mag;
                mag &= --rhs.mag;
                ++mag;
            }
            else if (sign > 0 && rhs.sign < 0)
            {
                // u | -v == -((v-1) & ~u + 1)
                sign = -1;
                --rhs.mag;
                mag = rhs.mag.and_not(mag);
                ++mag;
            }
            else if (sign < 0 && rhs.sign > 0)
            {
                --mag;
                mag = mag.and_not(rhs.mag);
                ++mag;
            }
            else
            {
                sign |= rhs.sign;
                mag |= rhs.mag;
            }
            return *this;
        }

        friend bool operator< (const Integer& u, const Integer& v)
        {
            return (u.sign < v.sign || (u.sign == v.sign &&
                ((u.sign < 0) ? (v.mag < u.mag) : (u.mag < v.mag))));
        }

        friend bool operator> (const Integer& u, const Integer& v)
        {
            return (v < u);
        }

        friend bool operator<= (const Integer& u, const Integer& v)
        {
            return !(v < u);
        }

        friend bool operator>= (const Integer& u, const Integer& v)
        {
            return !(u < v);
        }

        friend bool operator== (const Integer& u, const Integer& v)
        {
            return (u.sign == v.sign && u.mag == v.mag);
        }

        friend bool operator!= (const Integer& u, const Integer& v)
        {
            return (u.sign != v.sign || u.mag != v.mag);
        }

        std::int32_t signum() const
        {
            return sign;
        }

        Unsigned abs() const
        {
            return mag;
        }

        std::string to_string() const
        {
            std::ostringstream oss;
            oss << *this;
            return oss.str();
        }

        friend std::ostream& operator<< (std::ostream& os, const Integer& u)
        {
            if (u.sign < 0)
            {
                os << "-";
            }
            os << u.mag;
            return os;
        }

        friend std::istream& operator>> (std::istream& is, Integer& u)
        {
            char sign_ch = '\0';
            is >> sign_ch;
            if (is.good())
            {
                if (std::isdigit(sign_ch))
                {
                    is.putback(sign_ch);
                    sign_ch = '+';
                }
                if ((sign_ch == '+' || sign_ch == '-') &&
                    std::isdigit(is.peek()))
                {
                    is >> u.mag;
                    u.sign = (sign_ch == '-' ? -1 : 1);
                    if (u.mag == 0)
                    {
                        u.sign = 0;
                    }
                }
                else
                {
                    is.setstate(std::ios_base::failbit);
                }
            }
            else
            {
                is.setstate(std::ios_base::failbit);
            }
            return is;
        }

    private:
        std::int32_t sign;
        Unsigned mag;
    };
} // namespace math

#endif // MATH_INTEGER_H
