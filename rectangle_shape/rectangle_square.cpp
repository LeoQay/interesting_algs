#include <iostream>
#include <algorithm>
#include <set>
#include <vector>
#include <unordered_set>

using namespace std;

struct Line
{
    Line();
    Line (int _x1, int _x2);

    int len() const;
    bool is_none() const;
    bool is_in(const Line & obj) const;
    bool is_value_in(int value) const;
    Line operator&& (const Line & obj) const;
    bool is_intersection(const Line & obj) const;
    vector<Line> operator|| (const Line & obj) const;
    bool operator== (const Line & obj) const;

    int x1;
    int x2;
};


template<>
class std::hash<Line>
{
public:
    unsigned long long operator() (const Line & obj) const
    {
        long long result = 0;

        int * low = reinterpret_cast<int *>(&result);
        int * high = reinterpret_cast<int *>(&result + 4);

        *low = obj.x1;
        *high = obj.x2;

        std::hash<long long> hasher;

        return hasher(result);
    }
};


struct Dot
{
    int x;
    int y;
    Dot() { x = y = 0; }
    Dot (int _x, int _y)
    {
        this->x = _x;
        this->y = _y;
    }
};


Line::Line() { x1 = x2 = 0; }

Line::Line (int _x1, int _x2)
{
    this->x1 = min(_x1, _x2);
    this->x2 = max(_x1, _x2);
}

bool Line::operator== (const Line & obj) const
{
    return x1 == obj.x1 && x2 == obj.x2;
}

int Line::len() const
{
    return x2 - x1;
}

bool Line::is_none() const
{
    return len() == 0;
}

bool Line::is_in(const Line & obj) const
{
    return is_value_in(obj.x1) && is_value_in(obj.x2);
}

bool Line::is_value_in(int value) const
{
    return x1 <= value && value <= x2;
}

Line Line::operator&& (const Line & obj) const
{
    set<int> good;
    if (this->is_value_in(obj.x1)) good.insert(obj.x1);
    if (this->is_value_in(obj.x2)) good.insert(obj.x2);
    if (obj.is_value_in(x1)) good.insert(x1);
    if (obj.is_value_in(x2)) good.insert(x2);

    size_t size = good.size();

    if (size > 2)
    {
        throw exception();
    }

    if (size == 0)
    {
        return {0, 0};
    }

    int v[2], i = 0;
    for (int val : good)
    {
        v[i++] = val;
    }

    if (size == 1)
    {
        return {v[0], v[0]};
    }

    return {v[0], v[1]};
}

bool Line::is_intersection(const Line & obj) const
{
    return !(*this && obj).is_none();
}

vector<Line> Line::operator|| (const Line & obj) const
{
    vector<Line> result;

    if (this->is_none() || obj.is_none())
    {
        return result;
    }

    set<int> dots;

    dots.insert(x1);
    dots.insert(x2);
    dots.insert(obj.x1);
    dots.insert(obj.x2);

    size_t size = dots.size();
    int arr[4];
    size_t i = 0;

    for (int value : dots)
    {
        arr[i++] = value;
    }

    for (i = 1; i < size; i++)
    {
        result.emplace_back(arr[i - 1], arr[i]);
    }

    return result;
}


class Rectangle
{
public:
    explicit Rectangle();
    Rectangle(Dot _first, Dot _second);
    Rectangle(Line _x, Line _y);
    ~Rectangle() = default;

    int square() const;

    bool is_none() const;

    bool is_in(const Rectangle & obj) const;

    bool is_intersection(const Rectangle & obj) const;

    vector<Rectangle> operator- (const Rectangle & obj) const;

    Rectangle operator&& (const Rectangle & obj) const;

    bool operator== (const Rectangle & obj) const;

    // down_left and up_right dots of rectangle
    Line x;
    Line y;
};


template<>
class std::hash<Rectangle>
{
public:
    unsigned long long operator() (const Rectangle & obj) const
    {
        hash<Line> hasher;
        return hasher(obj.x) + hasher(obj.y);
    }
};


Rectangle::Rectangle() : x(), y() {}

Rectangle::Rectangle(Dot _first, Dot _second)
{
    *this = Rectangle{Line(_first.x, _second.x), Line(_first.y, _second.y)};
}

Rectangle::Rectangle(Line _x, Line _y) : x(_x), y(_y) {}

int Rectangle::square() const
{
    return x.len() * y.len();
}

bool Rectangle::operator== (const Rectangle & obj) const
{
    return x == obj.x && y == obj.y;
}

bool Rectangle::is_none() const
{
    return square() == 0;
}

bool Rectangle::is_in(const Rectangle & obj) const
{
    return this->x.is_in(obj.x) && this->y.is_in(obj.y);
}

Rectangle Rectangle::operator&& (const Rectangle & obj) const
{
    return {this->x && obj.x, this->y && obj.y};
}

bool Rectangle::is_intersection(const Rectangle & obj) const
{
    return !Rectangle(this->x && obj.x, this->y && obj.y).is_none();
}

vector<Rectangle> Rectangle::operator- (const Rectangle & obj) const
{
    vector<Rectangle> result;

    auto axis_x = x || obj.x;
    auto axis_y = y || obj.y;

    size_t size_x = axis_x.size();
    size_t size_y = axis_y.size();

    if (size_x == 0 || size_y == 0)
    {
        return result;
    }

    for (Line l_x : axis_x)
    {
        for (Line l_y : axis_y)
        {
            Rectangle pretend = {l_x, l_y};
            if (this->is_in(pretend) && !obj.is_in(pretend))
            {
                result.push_back(pretend);
            }
        }
    }

    return result;
}

/*
void print(vector<Rectangle> & v)
{
    cout << "V:\n";

    for (auto smth : v)
    {
        cout << smth.x.x1 << " " << smth.x.x2 << "\n";
        cout << smth.y.x1 << " " << smth.y.x2 << "\n";
    }
    cout.flush();
}
*/

int main()
{
    int n;
    scanf("%d", &n);

    vector<Rectangle> que;
    unordered_set<Rectangle> result;

    int x1, y1, x2, y2;

    for (int i = 0; i < n; i++)
    {
        scanf("%d", &x1);
        scanf("%d", &y1);
        scanf("%d", &x2);
        scanf("%d", &y2);
        que.emplace_back(Dot(x1, y1), Dot(x2, y2));
    }

    while (!que.empty())
    {
        Rectangle cur = que.back();
        que.pop_back();

        if (cur.is_none()) continue;

        if (result.find(cur) != result.end()) continue;

        bool is_cont = false;

        for (auto & it : result)
        {
            if (it.is_intersection(cur))
            {
                if (it.is_in(cur))
                {
                    is_cont = true;
                    break;
                }

                auto elems = cur - it;

                for (auto & elem : elems)
                {
                    que.push_back(elem);
                }

                is_cont = true;
                break;
            }
        }

        if (is_cont) continue;

        result.insert(cur);
    }

    long long square = 0;

    for (auto & it : result)
    {
        square += static_cast<long long>(it.square());
    }

    printf("%lld\n", square);

    return 0;
}
