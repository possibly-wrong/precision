#ifndef MATH_RATIONAL_H
#define MATH_RATIONAL_H

#include "math_Integer.h"

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
            is >> u.a;
            if (is.good() && is.peek() == '/')
            {
                is.ignore(1);
                Unsigned d = 1;
                is >> d;
                u.b = d;
                u.reduce();
            }
            else
            {
                u.b = 1;
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
                throw std::domain_error("Error: Rational::divbyzero");
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
