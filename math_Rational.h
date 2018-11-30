#ifndef MATH_RATIONAL_H
#define MATH_RATIONAL_H

#include "math_Integer.h"
#include <cmath>
#include <cfloat>
#include <map>
#include <vector>

namespace math
{
    class Rational
    {
    public:
        Rational(std::int32_t numerator = 0, const Integer& denominator = 1) :
            a(numerator),
            b(denominator)
        {
            reduce();
        }

        Rational(const Unsigned& numerator, const Integer& denominator = 1) :
            a(numerator),
            b(denominator)
        {
            reduce();
        }

        Rational(const Integer& numerator, const Integer& denominator = 1) :
            a(numerator),
            b(denominator)
        {
            reduce();
        }

        Rational(double x) :
            a(0),
            b(1)
        {
            if (!std::isfinite(x))
            {
                throw std::range_error("Error: Rational::double");
            }
            int exponent = 0;
            x = std::frexp(x, &exponent);
            for (int j = 0; x != 0 && j < DBL_MANT_DIG; ++j)
            {
                double bit = 0;
                x = std::modf(x * 2, &bit);
                a <<= 1;
                a += static_cast<std::int32_t>(bit);
                --exponent;
            }
            if (exponent > 0)
            {
                a <<= exponent;
            }
            else if (exponent < 0)
            {
                b <<= -exponent;
            }
            reduce();
        }

        Rational(const std::string& s) :
            a(),
            b()
        {
            std::istringstream iss(s);
            iss >> *this;
            if (iss.fail() || !iss.eof())
            {
                throw std::runtime_error("Error: Rational::string");
            }
        }

        Rational(const Rational& copy) :
            a(copy.a),
            b(copy.b)
        {
            // empty
        }

        Rational& operator= (const Rational& rhs)
        {
            a = rhs.a;
            b = rhs.b;
            return *this;
        }

        Rational operator+ () const
        {
            return *this;
        }

        Rational operator- () const
        {
            Rational w(*this);
            w.a = -w.a;
            return w;
        }

        Rational operator++ (int)
        {
            Rational w(*this);
            ++(*this);
            return w;
        }

        Rational& operator++ ()
        {
            a += b;
            return *this;
        }

        Rational operator-- (int)
        {
            Rational w(*this);
            --(*this);
            return w;
        }

        Rational& operator-- ()
        {
            a -= b;
            return *this;
        }

        friend Rational operator+ (Rational u, const Rational& v)
        {
            u += v;
            return u;
        }

        Rational& operator+= (const Rational& rhs)
        {
            a *= rhs.b;
            a += b * rhs.a;
            b *= rhs.b;
            reduce();
            return *this;
        }

        friend Rational operator- (Rational u, const Rational& v)
        {
            u -= v;
            return u;
        }

        Rational& operator-= (const Rational& rhs)
        {
            a *= rhs.b;
            a -= b * rhs.a;
            b *= rhs.b;
            reduce();
            return *this;
        }

        friend Rational operator* (Rational u, const Rational& v)
        {
            u *= v;
            return u;
        }

        Rational& operator*= (const Rational& rhs)
        {
            a *= rhs.a;
            b *= rhs.b;
            reduce();
            return *this;
        }

        friend Rational operator/ (Rational u, const Rational& v)
        {
            u /= v;
            return u;
        }

        Rational& operator/= (const Rational& rhs)
        {
            a *= rhs.b;
            b *= rhs.a;
            reduce();
            return *this;
        }

        friend bool operator< (const Rational& u, const Rational& v)
        {
            return (u.a * v.b < u.b * v.a);
        }

        friend bool operator> (const Rational& u, const Rational& v)
        {
            return (u.a * v.b > u.b * v.a);
        }

        friend bool operator<= (const Rational& u, const Rational& v)
        {
            return (u.a * v.b <= u.b * v.a);
        }

        friend bool operator>= (const Rational& u, const Rational& v)
        {
            return (u.a * v.b >= u.b * v.a);
        }

        friend bool operator== (const Rational& u, const Rational& v)
        {
            return (u.a == v.a && u.b == v.b);
        }

        friend bool operator!= (const Rational& u, const Rational& v)
        {
            return (u.a != v.a || u.b != v.b);
        }

        Integer numerator() const
        {
            return a;
        }

        Integer denominator() const
        {
            return b;
        }

        double to_double() const
        {
            // Compute a/b = n/d * 2^exponent, with 1/4 < n/d < 1.
            Unsigned n = a.abs();
            Unsigned d = b.abs();
            int exponent = n.bits() - d.bits() + 1;
            if (exponent > 0)
            {
                d <<= exponent;
            }
            else if (exponent < 0)
            {
                n <<= -exponent;
            }

            // Shift to ensure 1/2 <= n/d < 1.
            Unsigned r = n << 1;
            if (r < d)
            {
                n = r;
                --exponent;
            }

            // Reduce mantissa bits for subnormals.
            int bits = DBL_MANT_DIG;
            if (exponent < DBL_MIN_EXP)
            {
                bits = std::max(0, bits - (DBL_MIN_EXP - exponent));
            }

            // Shift to integer mantissa and round to even.
            n <<= bits;
            exponent -= bits;
            n.divide(d, n, r);
            r <<= 1;
            if (r > d || (r == d && (n & 1) != 0))
            {
                ++n;
            }

            // Convert to double (assume BITS <= DBL_MANT_DIG <= 2*BITS).
            double x = std::ldexp(std::ldexp((n >> Unsigned::BITS).to_uint(),
                Unsigned::BITS) + n.to_uint(), exponent);
            return ((a.signum() < 0) ? -x : x);
        }

        Rational round(size_t digits = 0) const
        {
            // Compute d = 10 ^ digits.
            Unsigned n = 10;
            Unsigned d = 1;
            for (size_t j = digits; j != 0; j >>= 1)
            {
                if ((j & 1) != 0)
                {
                    d *= n;
                }
                n *= n;
            }

            // Shift decimal point and round to even.
            n = a.abs() * d;
            Unsigned r;
            n.divide(b.abs(), n, r);
            r <<= 1;
            if (r > b.abs() || (r == b.abs() && (n & 1) != 0))
            {
                ++n;
            }
            return Rational((a.signum() < 0) ? -Integer(n) : n, d);
        }

        std::string to_string(size_t digits) const
        {
            std::ostringstream oss;
            Rational w = round(digits);
            if (w.a.signum() < 0)
            {
                oss << "-";
            }
            Unsigned q, r = w.a.abs();
            r.divide(w.b.abs(), q, r);
            oss << q;
            if (r != 0)
            {
                oss << ".";
                for (size_t j = 0; r != 0 && j < digits; ++j)
                {
                    r *= 10;
                    r.divide(w.b.abs(), q, r);
                    oss << q;
                }
            }
            return oss.str();
        }

        std::string to_precise_string() const
        {
            std::string s;
            if (a.signum() < 0) s.append("-");
            Unsigned q, r = a.abs();
            r.divide(b.abs(), q, r);
            s.append(q.to_string());
            if (r != 0)
            {
                s.append(".");
                std::map<Integer, std::map<Integer, unsigned long>> h;
                while (r != 0)
                {
                    r *= 10;
                    r.divide(b.abs(), q, r);
                    std::map<Integer, unsigned long>* rp = nullptr;
                    if (h.find(r) != h.end()) rp = &h.at(r);
                    if (rp != nullptr && rp->find(q) != rp->end())
                        return s.substr(0, rp->at(q)) + "(" +
                               s.substr(rp->at(q)) + ")";
                    if (rp == nullptr) { h[r] = {}; rp = &h.at(r); }
                    if (rp->find(q) == rp->end()) (*rp)[q] = s.length();
                    s.append(q.to_string());
                }
            }
            return s;
        }

        std::string to_string() const
        {
            std::ostringstream oss;
            oss << *this;
            return oss.str();
        }

        friend std::ostream& operator<< (std::ostream& os, const Rational& u)
        {
            os << u.a;
            if (u.b != 1)
            {
                os << "/" << u.b;
            }
            return os;
        }

        friend std::istream& operator>> (std::istream& is, Rational& u)
        {
            char sign = '\0';
            is >> sign;
            if (is.good())
            {
                if (std::isdigit(sign))
                {
                    is.putback(sign);
                    sign = '+';
                }
                if ((sign == '+' || sign == '-') && std::isdigit(is.peek()))
                {
                    is >> u.a;
                    u.b = 1;
                    if (is.good())
                    {
                        switch (is.peek())
                        {
                        case '/':
                        {
                            is.ignore(1);
                            Unsigned d = 1;
                            is >> d;
                            u.b = d;
                            break;
                        }
                        case '.':
                        {
                            is.ignore(1);
                            char digit = '\0';
                            while (std::isdigit(is.peek()))
                            {
                                is >> digit;
                                u.a = 10 * u.a + (digit - '0');
                                u.b *= 10;
                            }
                            break;
                        }
                        default:
                            break;
                        }
                    }
                    if (sign == '-')
                    {
                        u.a = -u.a;
                    }
                    u.reduce();
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
        Integer a;
        Integer b;

        void reduce()
        {
            switch (b.signum())
            {
            case 0:
                throw std::overflow_error("Error: Rational::overflow");
                break;
            case -1:
                a = -a;
                b = -b;
                break;
            default:
                break;
            }
            Unsigned d(a.abs());
            Unsigned c(b.abs());
            while (c != 0)
            {
                Unsigned r = d % c;
                d = c;
                c = r;
            }
            a /= d;
            b /= d;
        }
    };
} // namespace math

#endif // MATH_RATIONAL_H
