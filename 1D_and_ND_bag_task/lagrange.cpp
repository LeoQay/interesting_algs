#include <iostream>
#include <algorithm>
#include <vector>
#include <cmath>


namespace BagTask
{
    using FLOAT = double;


    class Bag1DSolver
    {
    public:
        struct BagSubTask
        {
            BagSubTask() : fixed(false), prev(-1), acc_weights(0), acc_price(0) {}

            BagSubTask(int fixed_, int previous_, FLOAT accumulated_weights_, FLOAT accumulated_price_) :
                fixed(fixed_), prev(previous_), acc_weights(accumulated_weights_),
                acc_price(accumulated_price_) {}

            bool fixed;
            int prev;
            FLOAT acc_weights;
            FLOAT acc_price;
        };

        struct Result
        {
            Result() : x(), maximum(0) {}

            std::vector<bool> x;
            FLOAT maximum;
        };

        static Result solve(const FLOAT * prices, const FLOAT * weights, int n, FLOAT C)
        {
            std::vector<std::vector<BagSubTask>> lists(n + 1);
            std::vector<int> not_negative(n);
            lists[0].emplace_back(-1, -1, 0, 0);

            int lists_index = 0;
            for (int i = 0; i < n; i++)
            {
                if (prices[i] < 0) continue;

                auto & cur = lists[lists_index];
                auto & next = lists[lists_index + 1];
                not_negative[lists_index] = i;
                lists_index++;

                int tasks_number = (int) cur.size();
                int index_cur = 0, index_temp = 0;
                while (true)
                {
                    if (index_cur < tasks_number && index_temp < tasks_number)
                    {
                        auto new_weight = cur[index_temp].acc_weights + weights[i];
                        auto new_price = cur[index_temp].acc_price + prices[i];

                        if (new_weight > C) {
                            index_temp++;
                            continue;
                        }

                        if (cur[index_cur].acc_weights <= new_weight) {
                            if (cur[index_cur].acc_price >= new_price) {
                                index_temp++;
                            } else {
                                next.emplace_back(0, index_cur, cur[index_cur].acc_weights, cur[index_cur].acc_price);
                                index_cur++;
                            }
                        } else {
                            if (new_price >= cur[index_cur].acc_price) {
                                index_cur++;
                            } else {
                                next.emplace_back(1, index_temp, new_weight, new_price);
                                index_temp++;
                            }
                        }
                    }
                    else if (index_cur < tasks_number)
                    {
                        for (; index_cur < tasks_number; index_cur++)
                        {
                            next.emplace_back(0, index_cur, cur[index_cur].acc_weights, cur[index_cur].acc_price);
                        }
                        break;
                    }
                    else if (index_temp < tasks_number)
                    {
                        for (; index_temp < tasks_number; index_temp++)
                        {
                            auto new_weight = cur[index_temp].acc_weights + weights[i];
                            auto new_price = cur[index_temp].acc_price + prices[i];

                            if (new_weight <= C) next.emplace_back(1, index_temp, new_weight, new_price);
                        }
                        break;
                    }
                    else break;
                }
            }

            Result result;
            result.maximum = lists[lists_index].back().acc_price;
            result.x.resize(n, false);

            int current = (int) lists[lists_index].size() - 1;
            for (int i = lists_index; i >= 1; i--)
            {
                auto & elem = lists[i][current];
                if (elem.fixed == 1) result.x[not_negative[i - 1]] = true;
                current = elem.prev;
            }

            return result;
        }
    };


    class BagNDLagrangeRelaxationSearcher
    {
    public:
        struct Result
        {
            Result() : minimum(0), lambda() {}

            FLOAT minimum;
            std::vector<FLOAT> lambda;
        };

        BagNDLagrangeRelaxationSearcher() :
        lambda_(), prices_(), bag_1d_result_(), gradient_(), weights_(nullptr), constraint_(0),
        constant_(0), n_(0), m_(0), index_(0), _prices(nullptr), _weights(nullptr),
        _constraints(nullptr) {}


        /*
         * prices is vector (n)
         * weights is matrix (m, n)
         * constraints is vector (m)
         * index determines which constraint is left for 1D bag
         */
        Result search(
            const std::vector<FLOAT> & prices,
            const std::vector<std::vector<FLOAT>> & weights,
            const std::vector<FLOAT> & constraints,
            int index,
            int max_iterations)
        {
            init_search(prices, weights, constraints, index);

            // compute values for lambda initialization
            make_task();
            solve_task();
            eval_history_.push_back(constant_);

            int count_eval_bad_update = 0;

            long long rate = 1;
            int degree = 0;

            for (int iter = 1; iter <= max_iterations; iter++)
            {
                compute_gradient();
                FLOAT gradient_norm = compute_gradient_norm();

                if (fabs(gradient_norm) < GRADIENT_EPS) break;

                GD_step(iter, gradient_norm);

                make_task();
                solve_task();
                eval_history_.push_back(constant_);

                FLOAT step = eval_history_[iter] - eval_history_[iter - 1];

                if (step < EVAL_GOOD_STEP) {
                    if (degree > 0) {
                        rate /= 2;
                        degree--;
                    }
                } else {
                    if (degree <= 62) {
                        degree++;
                        rate *= 2;
                    }
                }

                if (step >= 0) {
                    count_eval_bad_update++;
                } else {
                    count_eval_bad_update = 0;
                }

                if (count_eval_bad_update >= EVAL_MAX_BAD_COUNT) break;
            }

            auto result = build_result();

            clear();

            return result;
        }

    private:

        Result build_result()
        {
            Result result;
            result.minimum = eval_history_.back();
            for (int j = 0; j < m_; j++)
            {
                if (j != index_) result.lambda.push_back(lambda_[j]);
            }
            return result;
        }

        void GD_step(long long rate, FLOAT gradient_norm)
        {
            for (int j = 0; j < m_; j++)
            {
                if (j == index_) continue;
                lambda_[j] -= gradient_[j] / gradient_norm / (FLOAT) rate;
                if (lambda_[j] < 0) lambda_[j] = 0;
            }
        }

        FLOAT compute_gradient_norm()
        {
            FLOAT gradient_norm_2 = 0;
            for (int j = 0; j < m_; j++)
            {
                if (j == index_) continue;
                gradient_norm_2 += gradient_[j] * gradient_[j];
            }
            return sqrt(gradient_norm_2);
        }

        void compute_gradient()
        {
            auto & weights = *_weights;
            auto & constraints = *_constraints;

            for (int j = 0; j < m_; j++)
            {
                if (j == index_) continue;
                gradient_[j] = constraints[j];
                FLOAT sum = 0;
                for (int i = 0; i < n_; i++)
                {
                    if (bag_1d_result_.x[i]) sum += weights[j][i];
                }
                gradient_[j] -= sum;
            }
        }

        void solve_task()
        {
            bag_1d_result_ = Bag1DSolver::solve(prices_.data(), weights_, n_, constraint_);
            constant_ += bag_1d_result_.maximum;
        }

        void make_task()
        {
            auto & weights = *_weights;
            auto & prices = *_prices;
            auto & constraints = *_constraints;

            for (int i = 0; i < n_; i++)
            {
                FLOAT sum = 0;
                for (int j = 0; j < m_; j++)
                {
                    if (j == index_) continue;
                    sum += lambda_[j] * weights[j][i];
                }
                prices_[i] = prices[i] - sum;
            }

            constant_ = 0;
            for (int j = 0; j < m_; j++)
            {
                if (j == index_) continue;
                constant_ += lambda_[j] * constraints[j];
            }
        }


        void init_search(
            const std::vector<FLOAT> & prices,
            const std::vector<std::vector<FLOAT>> & weights,
            const std::vector<FLOAT> & constraints,
            int index)
        {
            n_ = (int) prices.size();
            m_ = (int) weights.size();

            lambda_.resize(m_, 1);
            prices_.resize(n_);
            gradient_.resize(m_);

            weights_ = weights[index].data();
            constraint_ = constraints[index];

            index_ = index;
            _prices = &prices;
            _weights = &weights;
            _constraints = &constraints;
        }

        void clear()
        {
            lambda_.clear();
            bag_1d_result_.x.clear();
            gradient_.clear();
            eval_history_.clear();

            prices_.clear();
            weights_ = nullptr;
            constraint_ = 0;
            constant_ = 0;

            n_ = 0;
            m_ = 0;
            index_ = 0;
            _prices = nullptr;
            _weights = nullptr;
            _constraints = nullptr;
        }

        ///////////////////////////////////////////////////////
        /// Constants
        static constexpr FLOAT GRADIENT_EPS = 0.000001;
        static constexpr int EVAL_MAX_BAD_COUNT = 1000;
        static constexpr FLOAT EVAL_GOOD_STEP = 0.01;
        ///////////////////////////////////////////////////////


        ////////////////////////////////////////////////////////
        /// Parameters throwout optimization
        /// ( lambda_[index] and gradient_[index] not used )
        std::vector<FLOAT> lambda_;
        Bag1DSolver::Result bag_1d_result_;
        std::vector<FLOAT> gradient_;
        std::vector<FLOAT> eval_history_;
        ////////////////////////////////////////////////////////


        ////////////////////////////////////////////////////////
        /// TASK to 1D solver
        std::vector<FLOAT> prices_;
        const FLOAT * weights_;
        FLOAT constraint_;
        FLOAT constant_;
        ////////////////////////////////////////////////////////


        ////////////////////////////////////////////////////////
        /// Saved user input
        int n_;
        int m_;
        int index_;
        const std::vector<FLOAT> * _prices;
        const std::vector<std::vector<FLOAT>> * _weights;
        const std::vector<FLOAT> * _constraints;
        ////////////////////////////////////////////////////////
    };
}




int main()
{
    int n, m;
    std::cin >> n >> m;

    std::vector<BagTask::FLOAT> constraints(m);
    for (int i = 0; i < m; i++) std::cin >> constraints[i];

    std::vector<BagTask::FLOAT> prices(n);
    for (int i = 0; i < n; i++) std::cin >> prices[i];

    std::vector<std::vector<BagTask::FLOAT>> weights(m);
    for (int i = 0; i < m; i++) weights[i].resize(n);

    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < m; j++) std::cin >> weights[j][i];
    }

    BagTask::BagNDLagrangeRelaxationSearcher searcher;

    auto result = searcher.search(prices, weights, constraints, m - 1, 50000);

    std::cout << result.minimum << '\n';

    for (auto var : result.lambda)
    {
        std::cout << var << '\n';
    }

    return 0;
}
